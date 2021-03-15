##
## EPITECH PROJECT, 2020
## Doop
## File description:
## Makefile for the doop
##

SRC		=	wolfram.hs

NAME		=	wolfram

CC		=	ghc

RM		=	rm -f

OBJ		=	$(SRC:.hi=.o)

OBJ_O		=	$(SRC:.hs=.o)

OBJ_HI		=	$(SRC:.hs=.hi)

all:		$(NAME)

$(NAME):	$(OBJ)
		@printf "[\033[0;33mBuild\033[0m] % 32s\n" $(NAME) | tr ' ' '.'
		@$(CC) -o $(NAME) $(OBJ)

clean:
		@printf "[\033[0;31mDeleted\033[0m] % 30s\n" $(OBJ_O) | tr ' ' '.'
		@printf "[\033[0;31mDeleted\033[0m] % 30s\n" $(OBJ_HI) | tr ' ' '.'
		@$(RM) $(OBJ_HI)
		@$(RM) $(OBJ_O)

fclean: 	clean
		@$(RM) $(NAME)
		@printf "[\033[0;31mDeleted\033[0m] % 30s\n" $(NAME) | tr ' ' '.'

re:		fclean all

.PHONY:	all clean fclean re
