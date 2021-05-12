##
# Exercici 2 - Pràctica 1
#
# @file
# @version 0.1
lexer : app/Lexer.x 
	alex app/Lexer.x

parser : app/Parser.y
	happy app/Parser.y

all : lexer parser

clean:
	rm -f *.o *.hi Lexer.hs Parser.hs *.bin

# end
