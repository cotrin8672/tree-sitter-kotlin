#include "tree_sitter/parser.h"

#include <string.h>
#include <wctype.h>

enum TokenType {
    SEMI,
    CLASS_MEMBER_SEMI,
    BLOCK_COMMENT,
    NOT_IS,
    IN,
    Q_DOT,
    MULTILINE_STRING_CONTENT,
    CONSTRUCTOR,
    GET,
    SET,
    DOLLAR,
    GENERIC_LT,
    CONTEXT_KW,
    CONTEXT_TYPE_KW,
};

#define MAX_WORD_SIZE 16
#define MAX_WORDS 16

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

// Skip whitespace and both comment forms (lookahead only).
static void skip_trivia(TSLexer *lexer) {
    for (;;) {
        if (iswspace(lexer->lookahead)) {
            advance(lexer);
            continue;
        }
        if (lexer->lookahead == '/') {
            advance(lexer);
            if (lexer->lookahead == '/') {
                while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
                    advance(lexer);
                }
                continue;
            }
            if (lexer->lookahead == '*') {
                advance(lexer);
                unsigned depth = 1;
                while (!lexer->eof(lexer) && depth > 0) {
                    if (lexer->lookahead == '*') {
                        advance(lexer);
                        if (lexer->lookahead == '/') {
                            advance(lexer);
                            depth--;
                        }
                    } else if (lexer->lookahead == '/') {
                        advance(lexer);
                        if (lexer->lookahead == '*') {
                            advance(lexer);
                            depth++;
                        }
                    } else {
                        advance(lexer);
                    }
                }
                continue;
            }
            return;
        }
        return;
    }
}

// Scan a balanced parenthesized region (the cursor is at the opening `(`). Allows anything
// types and parameter lists may contain; bails on quotes/braces/semicolons/eof.
static bool scan_balanced_parens(TSLexer *lexer, unsigned cap) {
    unsigned depth = 0;
    unsigned scanned = 0;
    while (!lexer->eof(lexer) && scanned++ < cap) {
        int32_t c = lexer->lookahead;
        if (c == '(') {
            depth++;
            advance(lexer);
            continue;
        }
        if (c == ')') {
            depth--;
            advance(lexer);
            if (depth == 0) {
                return true;
            }
            continue;
        }
        if (c == '{' || c == '}' || c == ';' || c == '"' || c == '\'') {
            return false;
        }
        advance(lexer);
    }
    return false;
}

static bool scan_word_at(TSLexer *lexer, const char *word) {
    for (uint8_t i = 0; word[i] != '\0'; i++) {
        if (lexer->lookahead != word[i]) {
            return false;
        }
        advance(lexer);
    }
    return !iswalnum(lexer->lookahead) && lexer->lookahead != '_';
}

// After `fun`, decide whether an anonymous function follows: `(`, type parameters, or a
// receiver (`T.()`) are anonymous; a plain name is a named declaration.
static bool scan_anonymous_fun_tail(TSLexer *lexer) {
    skip_trivia(lexer);
    if (lexer->lookahead == '(') {
        return true;
    }
    if (lexer->lookahead == '<') {
        unsigned depth = 0;
        unsigned scanned = 0;
        while (!lexer->eof(lexer) && scanned++ < 1024) {
            if (lexer->lookahead == '<') {
                depth++;
            } else if (lexer->lookahead == '>') {
                depth--;
                if (depth == 0) {
                    advance(lexer);
                    skip_trivia(lexer);
                    return lexer->lookahead == '(';
                }
            }
            advance(lexer);
        }
        return false;
    }
    if (iswalpha(lexer->lookahead) || lexer->lookahead == '_') {
        while (iswalnum(lexer->lookahead) || lexer->lookahead == '_') {
            advance(lexer);
        }
        skip_trivia(lexer);
        if (lexer->lookahead != '.') {
            return false;
        }
        advance(lexer);
        skip_trivia(lexer);
        return lexer->lookahead == '(';
    }
    return false;
}

// Verify that `context` (already consumed, token end marked) opens a context-parameters
// clause: a parenthesized list followed by a declaration keyword (declaration token) or by
// a function-type tail / anonymous `fun` (type token). Returns the token to emit, or -1.
static int verify_context_clause(TSLexer *lexer, bool decl_valid, bool type_valid) {
    skip_trivia(lexer);
    if (lexer->lookahead != '(' || !scan_balanced_parens(lexer, 4096)) {
        return -1;
    }
    skip_trivia(lexer);
    switch (lexer->lookahead) {
        case 'f':
            if (!scan_word_at(lexer, "fun")) {
                return -1;
            }
            if (type_valid && scan_anonymous_fun_tail(lexer)) {
                return CONTEXT_TYPE_KW;
            }
            return decl_valid ? CONTEXT_KW : -1;
        case 'v':
            if (!decl_valid) {
                return -1;
            }
            return (scan_word_at(lexer, "val") || scan_word_at(lexer, "var")) ? CONTEXT_KW : -1;
        case '(': {
            if (!type_valid || !scan_balanced_parens(lexer, 1024)) {
                return -1;
            }
            skip_trivia(lexer);
            return lexer->lookahead == '-' ? CONTEXT_TYPE_KW : -1;
        }
        default: {
            if (!type_valid || !(iswalpha(lexer->lookahead) || lexer->lookahead == '_')) {
                return -1;
            }
            // Possible receiver type of a function type: `context(A) B.C.() -> R`
            while (iswalnum(lexer->lookahead) || lexer->lookahead == '_' || lexer->lookahead == '.') {
                advance(lexer);
            }
            if (lexer->lookahead != '(' || !scan_balanced_parens(lexer, 1024)) {
                return -1;
            }
            skip_trivia(lexer);
            return lexer->lookahead == '-' ? CONTEXT_TYPE_KW : -1;
        }
    }
}

// Lookahead check for `<` starting call-site type arguments: scan to the matching `>`
// (tracking nesting and `->` arrows in function types) and require `(` or `{` next.
// The token itself is only the `<`; everything past it is pure lookahead.
static bool scan_generic_lt(TSLexer *lexer) {
    advance(lexer); // consume `<`
    lexer->mark_end(lexer);

    unsigned depth = 1;
    unsigned scanned = 0;
    while (!lexer->eof(lexer) && scanned++ < 4096) {
        int32_t c = lexer->lookahead;
        if (c == '<') {
            depth++;
            advance(lexer);
            continue;
        }
        if (c == '-') {
            advance(lexer);
            if (lexer->lookahead == '>') {
                advance(lexer); // `->` inside a function type is not a closing bracket
            }
            continue;
        }
        if (c == '>') {
            depth--;
            advance(lexer);
            if (depth == 0) {
                while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
                    skip(lexer);
                }
                return lexer->lookahead == '(' || lexer->lookahead == '{';
            }
            continue;
        }
        if (iswalnum(c) || c == '_' || c == '.' || c == ',' || c == '?' || c == '*' ||
            c == '(' || c == ')' || c == ':' || c == '@' || c == '[' || c == ']' ||
            iswspace(c)) {
            advance(lexer);
            continue;
        }
        return false;
    }
    return false;
}

static bool scan_word(TSLexer *lexer, const char *const word) {
    for (uint8_t i = 0; word[i] != '\0'; i++) {
        if (lexer->lookahead != word[i]) {
            return false;
        }
        skip(lexer);
    }
    return true;
}

static bool scan_words(TSLexer *lexer, const char words[MAX_WORDS][MAX_WORD_SIZE], char scanned_word[16],
                       uint8_t *index) {
    if (!scanned_word[0]) {
        for (uint8_t i = 0; i < MAX_WORD_SIZE - 1; i++) {
            if (!iswalpha(lexer->lookahead)) {
                if (i == 0) {
                    return false;
                }
                break;
            }
            scanned_word[i] = (char)lexer->lookahead;
            skip(lexer);
        }
    }

    for (uint8_t i = 0; i < MAX_WORDS; i++) {
        if (strncmp(scanned_word, words[i], MAX_WORD_SIZE) == 0) {
            if (index != NULL) {
                *index = i;
            }
            return true;
        }
    }

    return false;
}

void *tree_sitter_kotlin_external_scanner_create() { return NULL; }

void tree_sitter_kotlin_external_scanner_destroy(void *payload) {}

unsigned tree_sitter_kotlin_external_scanner_serialize(void *payload, char *buffer) { return 0; }

void tree_sitter_kotlin_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {}

bool tree_sitter_kotlin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    // `context` clauses are only recognized where a semicolon cannot intervene (statement
    // starts after an inserted semi, type positions, file start). Skip leading whitespace —
    // the scanner runs before extras are skipped — then verify the full clause shape.
    bool context_decl = valid_symbols[CONTEXT_KW];
    bool context_type = valid_symbols[CONTEXT_TYPE_KW];
    if ((context_decl || context_type) && !valid_symbols[SEMI] && !valid_symbols[CLASS_MEMBER_SEMI]) {
        while (iswspace(lexer->lookahead)) {
            skip(lexer);
        }
        bool matches = lexer->lookahead == 'c';
        if (matches) {
            const char *word = "context";
            for (uint8_t i = 0; word[i] != '\0' && matches; i++) {
                if (lexer->lookahead != word[i]) {
                    matches = false;
                }
                advance(lexer);
            }
        }
        if (matches && !iswalnum(lexer->lookahead) && lexer->lookahead != '_') {
            lexer->mark_end(lexer);
            int token = verify_context_clause(lexer, context_decl, context_type);
            if (token >= 0) {
                lexer->result_symbol = token;
                return true;
            }
        }
        // Not a context clause: fall through so the other external tokens (`!is`, comments,
        // generic `<`, ...) are still considered. If none match, the false return resets the
        // lexer and the internal lexer produces an identifier.
    }

    if (valid_symbols[GENERIC_LT] && lexer->lookahead == '<') {
        if (scan_generic_lt(lexer)) {
            lexer->result_symbol = GENERIC_LT;
            return true;
        }
        // A `<` that does not open call-site type arguments is the comparison operator; let
        // the internal lexer take it. Returning false resets the lexer to this position.
        return false;
    }
    if (valid_symbols[MULTILINE_STRING_CONTENT]) {
        bool did_advance = false;
        lexer->result_symbol = MULTILINE_STRING_CONTENT;
        while (!lexer->eof(lexer)) {
            switch (lexer->lookahead) {
                case '$':
                    lexer->mark_end(lexer);
                    advance(lexer);
                    if (iswalpha(lexer->lookahead) || lexer->lookahead == '{') {
                        return did_advance;
                    }
                    did_advance = true;
                    break;
                case '"':
                    lexer->mark_end(lexer);
                    // 3 or 4 quotes means we're done
                    advance(lexer);
                    if (lexer->lookahead == '"') {
                        advance(lexer);
                        if (lexer->lookahead == '"') {
                            advance(lexer);
                            if (lexer->lookahead == '"') {
                                advance(lexer);
                            }
                            return did_advance;
                        }
                    }
                    did_advance = true;
                    break;
                default:
                    advance(lexer);
                    did_advance = true;
                    break;
            }
        }
    }

    if (valid_symbols[SEMI] || valid_symbols[CLASS_MEMBER_SEMI]) {
        lexer->result_symbol = valid_symbols[SEMI] ? SEMI : CLASS_MEMBER_SEMI;
        lexer->mark_end(lexer);
        bool saw_newline = false;
        for (;;) {
            if (lexer->eof(lexer)) {
                return true;
            }

            if (lexer->lookahead == ';') {
                advance(lexer);
                lexer->mark_end(lexer);
                return true;
            }

            if (!iswspace(lexer->lookahead)) {
                break;
            }

            if (lexer->lookahead == '\n') {
                skip(lexer);
                saw_newline = true;
                break;
            }

            if (lexer->lookahead == '\r') {
                skip(lexer);

                if (lexer->lookahead == '\n') {
                    skip(lexer);
                }

                saw_newline = true;
                break;
            }

            skip(lexer);
        }

        // Skip whitespace and comments
        while (iswspace(lexer->lookahead)) {
            skip(lexer);
        }
        if (lexer->lookahead == '/') {
            goto comment;
        }

        if (!saw_newline) {
            switch (lexer->lookahead) {
                case '!':
                    skip(lexer);
                    goto continue_not_is_from_semi;
                case '?':
                    if (valid_symbols[Q_DOT]) {
                        goto q_dot_from_semi;
                    }
                    return false;
                case 'i':
                    return scan_word(lexer, "import");
                case ';':
                    advance(lexer);
                    lexer->mark_end(lexer);
                    return true;
                // A closing brace terminates the current statement/member even without a
                // newline (`class A { fun m() {} }`). mark_end already sat at the start, so
                // this yields a zero-width semicolon before `}`.
                case '}':
                    return true;
                default:
                    return false;
            }
        }

        char scanned_word[16] = {0};
    _switch:
        switch (lexer->lookahead) {
            case ',':
            case '.':
            case ':':
            case '*':
            case '%':
            case '>':
            case '<':
            case '=':
            case '{':
            case '[':
            case '(':
            case '|':
            case '&':
            case '/':
                return false;
            // Insert a semicolon before `--` and `++`, but not before binary `+` or `-`.
            // Insert before +/-{float}
            case '+':
                skip(lexer);
                if (lexer->lookahead == '+') {
                    return true;
                }
                return iswdigit(lexer->lookahead);
            case '-':
                skip(lexer);
                if (lexer->lookahead == '-') {
                    return true;
                }
                return iswdigit(lexer->lookahead);
            // Don't insert a semicolon before `!=`, but do insert one before a unary `!`.
            case '!':
                skip(lexer);
                if (lexer->lookahead == 'i' && valid_symbols[NOT_IS]) {
                    skip(lexer);
                    if (lexer->lookahead == 's') {
                        skip(lexer);
                        if (!iswalnum(lexer->lookahead)) {
                            return true;
                        }
                    }
                }
                return lexer->lookahead != '=';
            case '?':
                if (valid_symbols[Q_DOT]) {
                    goto q_dot_from_semi;
                }
                return true;
            case 'e':
            case 'i':
            case 'g':
            case 's':
            case 'p':
            case 'a':
            case 'f':
            case 'o':
            case 'l':
            case 'v':
            case 'n':
            case 'c':
            case 'b':
            case 'w':
                while (scan_words(lexer,
                                  (const char[16][16]){"public", "private", "protected", "internal", "abstract",
                                                       "final", "open", "override", "lateinit", "vararg", "noinline",
                                                       "crossinline", "external", "suspend", "inline"},
                                  scanned_word, NULL)) {
                    memset(scanned_word, 0, MAX_WORD_SIZE);
                    while (iswspace(lexer->lookahead)) {
                        skip(lexer);
                    }
                }

                uint8_t index = -1;
                bool res = scan_words(
                    lexer,
                    (const char[16][16]){"else", "in", "instanceof", "get", "set", "constructor", "by", "as", "where"},
                    scanned_word, &index);

                // If `CLASS_MEMBER_SEMI` is valid, we found a secondary constructor and so we want to insert a semi, OR
                // we found a variable named constructor whose field is being accessed
                if (index == 5) {
                    while (iswspace(lexer->lookahead)) {
                        skip(lexer);
                    }
                    if (valid_symbols[CLASS_MEMBER_SEMI] || lexer->lookahead == '.' || lexer->lookahead == '=') {
                        return true;
                    }
                }
                // Ordinarily, we should not insert a semicolon if there is an `else` on the next line,
                // except for when it's a 'when entry', which has a `->` after the `else`.
                else if (index == 0) {
                    while (iswspace(lexer->lookahead)) {
                        skip(lexer);
                    }
                    if (lexer->lookahead == '-') {
                        skip(lexer);
                        if (lexer->lookahead == '>') {
                            return true;
                        }
                    }
                }
                // If `get` was found and the keyword is not valid, return a semi since it's being used as an identifier
                else if (index == 3 && (!valid_symbols[GET] || lexer->lookahead == '[')) {
                    return true;
                }
                // If `set` was found and the keyword is not valid, return a semi since it's being used as an identifier
                else if (index == 4 && (!valid_symbols[SET] || lexer->lookahead == '[' || lexer->lookahead == '(' ||
                                        lexer->lookahead == '.')) {
                    if (lexer->lookahead == '(' && valid_symbols[SET]) {
                        // skip until the closing parenthesis
                        while (lexer->lookahead != ')' && !lexer->eof(lexer)) {
                            skip(lexer);
                        }
                        skip(lexer);

                        while (iswspace(lexer->lookahead)) {
                            if (lexer->lookahead == '\n') {
                                return true;
                            }
                            skip(lexer);
                        }
                        return false;
                    }
                    return true;
                }
                // If `in` was found and this specific external keyword is valid,
                // return a semi since it's being used in a range test
                else if (index == 1 && valid_symbols[IN]) {
                    return true;
                }
                return !res;
            case ';':
                advance(lexer);
                lexer->mark_end(lexer);
                return true;
            case '@':
                if (valid_symbols[CONSTRUCTOR]) {
                    while (!iswspace(lexer->lookahead)) {
                        skip(lexer);
                    }
                    while (iswspace(lexer->lookahead)) {
                        skip(lexer);
                    }
                    char ctor[12] = "constructor";
                    for (uint8_t i = 0; i < 11; i++) {
                        if (lexer->lookahead != ctor[i]) {
                            return true;
                        }
                        skip(lexer);
                    }
                    return false;
                }
                if (valid_symbols[GET] || valid_symbols[SET]) {
                    bool saw_paren = false;
                    while ((saw_paren ? lexer->lookahead != '\n' : !iswspace(lexer->lookahead))) {
                        skip(lexer);
                        if (lexer->lookahead == '(') {
                            saw_paren = true;
                        }
                        if (lexer->lookahead == ')') {
                            saw_paren = false;
                        }
                    }
                    while (iswspace(lexer->lookahead)) {
                        skip(lexer);
                    }
                    if (lexer->lookahead == '/') {
                        return true;
                    }
                    goto _switch;
                }
                return true;

            default:
                return true;
        }
    }

    while (iswspace(lexer->lookahead)) {
        skip(lexer);
    }

    if (valid_symbols[NOT_IS]) {
        if (lexer->lookahead == '!') {
            advance(lexer);
        continue_not_is_from_semi:
            if (lexer->lookahead == 'i') {
                advance(lexer);
                if (lexer->lookahead == 's') {
                    advance(lexer);
                    lexer->result_symbol = NOT_IS;
                    lexer->mark_end(lexer);
                    return !iswalnum(lexer->lookahead);
                }
            }
        }
    }

    if (valid_symbols[IN]) {
        if (lexer->lookahead == 'i') {
            advance(lexer);
            if (lexer->lookahead == 'n') {
                advance(lexer);
                lexer->result_symbol = IN;
                lexer->mark_end(lexer);
                return !iswalnum(lexer->lookahead);
            }
        }
    }

q_dot_from_semi:
    if (valid_symbols[Q_DOT]) {
        while (iswspace(lexer->lookahead)) {
            skip(lexer);
        }
        if (lexer->lookahead == '?') {
            advance(lexer);
            while (iswspace(lexer->lookahead)) {
                skip(lexer);
            }
            if (lexer->lookahead == '.') {
                advance(lexer);
                lexer->result_symbol = Q_DOT;
                lexer->mark_end(lexer);
                return true;
            }
        }
    }

comment:
    if (valid_symbols[DOLLAR]) {
        return false;
    }

    if (lexer->lookahead == '/') {
        advance(lexer);
        if (lexer->lookahead != '*') {
            return false;
        }
        advance(lexer);

        bool after_star = false;
        unsigned nesting_depth = 1;
        for (;;) {
            switch (lexer->lookahead) {
                case '\0':
                    return false;
                case '*':
                    advance(lexer);
                    after_star = true;
                    break;
                case '/':
                    if (after_star) {
                        advance(lexer);
                        after_star = false;
                        nesting_depth--;
                        if (nesting_depth == 0) {
                            lexer->result_symbol = BLOCK_COMMENT;
                            lexer->mark_end(lexer);
                            return true;
                        }
                    } else {
                        advance(lexer);
                        after_star = false;
                        if (lexer->lookahead == '*') {
                            nesting_depth++;
                            advance(lexer);
                        }
                    }
                    break;
                default:
                    advance(lexer);
                    after_star = false;
                    break;
            }
        }
    }

    return false;
}
