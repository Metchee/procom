##
## EPITECH PROJECT, 2025
## myPandoc
## File description:
## Makefile for the myPandoc project
##

NAME	= mypandoc

all: $(NAME)

$(NAME):
	stack build
	cp `stack path --local-install-root`/bin/$(NAME) ./

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
