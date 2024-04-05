##
## EPITECH PROJECT, 2024
## wolfram
## File description:
## makefile
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	Compressor

all		:
			stack build
			mv $(BINARY_PATH)/bin/$(NAME)-exe imageCompressor

clean	:
			stack clean

fclean	:	clean
			rm -f *.o
			rm -f imageCompressor

re		:	fclean all

.PHONY	:	all clean fclean re
