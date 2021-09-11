#include "lexer.h"

using namespace kaleidoscope;

int Lexer::getToken()
{                
    // skip whitespace
    while (isspace(lastChar))
    {
        lastChar = getchar();
    }

    // if the character is in the alphabet
    if (isalpha(lastChar))
    {
        // set our identifier to this char, and then append following chars until 
        // the next char is not alphanumeric
        identifierStr = lastChar;
        while (isalnum((lastChar = getchar())))
        {
            identifierStr += lastChar;
        }

        // return either of the defined keywords, or the identifier
        if (identifierStr == "def")
        {
            return Token::TOK_DEF;
        }
        if (identifierStr == "extern")
        {
            return Token::TOK_EXTERN;
        }
        if (identifierStr == "if")
        {
            return Token::TOK_IF;
        }
        if (identifierStr == "then")
        {
            return Token::TOK_THEN;
        }
        if (identifierStr == "else")
        {
            return Token::TOK_ELSE;
        }
        if (identifierStr == "for")
        {
            return Token::TOK_FOR;
        }
        if (identifierStr == "in")
        {
            return Token::TOK_IN;
        }

        return Token::TOK_IDENT;
    }

    // parse a number and a decimal point if it has it.
    // this is error prone, as it will try and parse a number with multiple
    // decimal points, and strtod() will just give the double until the first one
    if (isdigit(lastChar) || lastChar == '.')
    {
        std::string numStr;
        do
        {
            numStr += lastChar;
            lastChar = getchar();                    
        } while (isdigit(lastChar) || lastChar == '.');

        numValue = strtod(numStr.c_str(), nullptr);
        return Token::TOK_NUM;
    }

    // comment until end of line
    if (lastChar == '#')
    {
        do
        {
            lastChar = getchar();
        } while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

        if (lastChar != EOF)
        {
            return getToken();
        }
    }

    // check for end of file but don't eat the EOF
    if (lastChar == EOF)
    {
        return Token::TOK_EOF;
    }

    // otherwise just return the char as its ascii value
    int thisChar = lastChar;
    lastChar = getchar();
    return thisChar;
}