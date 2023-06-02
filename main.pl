main :- 
menu.

clear_screen :-
  tty_clear.

get_option(Option) :-
  write("\n\nInforme o número da opção desejada: "),
  read(Option).
    
menu :-
    clear_screen,
    writeln("\n---------------------------------     MENU     ---------------------------------\n\n"),
    writeln("                              ● Digite 1 para cadastrar jogador"),
    writeln("                              ● Digite 2 para jogar"),
    writeln("                              ● Digite 3 para visualizar o ranking"),
    writeln("                              ● Digite 4 para ver o modo história"),
    writeln("                              ● Digite 0 para sair"),
    get_option(Option),
    select_menu_option(Option).


select_menu_option(1) :- menu.
select_menu_option(2) :- menu.
select_menu_option(3) :- menu
select_menu_option(4) :- menu.
select_menu_option(_) :- show_invalid_option_message.