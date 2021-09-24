#pragma once

#include <string>
#include <map>

namespace kaleidoscope
{
	class Lexer
	{
	public:

		enum Token
		{
			TOK_EOF = -1,
			TOK_DEF = -2,
			TOK_EXTERN = -3,
			TOK_IDENT = -4,
			TOK_NUM = -5,
			TOK_IF = -6,
			TOK_THEN = -7,
			TOK_ELSE = -8,
			TOK_FOR = -9,
			TOK_IN = -10,
			TOK_UNARY = -11,
			TOK_BINARY = -12,
			TOK_VAR = -13
		};

		Lexer();
		~Lexer() = default;

	    int getToken();

        double getNumValue() const { return numValue; }

        const std::string& getIdentStr() const { return identifierStr; }

	private:
        
		bool isValidIdentChar(int c);

		int lastChar;
		double numValue;
		std::string identifierStr;

		std::map<std::string, Token> tokenLookup;
	};
}