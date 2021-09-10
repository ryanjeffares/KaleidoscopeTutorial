#pragma once

#include <string>

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
			TOK_NUM = -5
		};

		Lexer() : lastChar(' ') {}
		~Lexer() = default;

	    int getToken();

        double getNumValue() const { return numValue; }

        const std::string& getIdentStr() const { return identifierStr; }

	private:
        
		int lastChar;
		double numValue;
		std::string identifierStr;
	};
}