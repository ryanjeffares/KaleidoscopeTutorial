#pragma once

#include <string>
#include <map>
#include <fstream>
#include <cassert>

namespace kaleidoscope
{
	struct SourceLocation
	{
		SourceLocation(int l, int c) 
			: line(l), column(c) {}
		
		int line;
		int column;
	};

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

		Lexer(const std::string& file);
		~Lexer() = default;

	    int getToken();

        double getNumValue() const { return numValue; }

        const std::string& getIdentStr() const { return identifierStr; }

		static SourceLocation currentLocation;

	private:
        
		int advance();
		bool isValidIdentChar(int c);

		int lastChar;
		double numValue;
		std::string identifierStr;		

		std::map<std::string, Token> tokenLookup;

		SourceLocation lexerLocation;
	};
}