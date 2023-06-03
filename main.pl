% definição dos tipos de dados
:- dynamic jogador/2. % dynamic para permitir a modificação em tempo de execução
:- dynamic tabuleiro/1.

% função que inicia o programa
main :-
    exibirConteudoArquivo('introducao.txt'),
    sleep(3), % aguarda 5 segundos
    menu([]).

% ---------- INTRODUÇÃO E HISTÓRIA ----------

exibirConteudoArquivo(NomeArquivo) :-
    open(NomeArquivo, read, Stream),
    repeat,
    read_line_to_string(Stream, Conteudo),
    (   Conteudo \= end_of_file
    ->  format('~s~n', [Conteudo]),
        fail
    ;   true
    ),
    close(Stream).

% ---------- MENU ----------

% função que exibe o Menu
menu(Dados) :-
    shell(clear), % limpa a tela
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n ● Digite 1 para cadastrar jogador'),
    writeln('\n ● Digite 2 para jogar'),
    writeln('\n ● Digite 3 para visualizar o ranking'),
    writeln('\n ● Digite 4 para ver o modo história'),
    writeln('\n ● Digite 0 para sair '),
    write('\n→ Opção: '),
    read_line_to_string(user_input, Op),
    executarOpcao(Dados, Op).

% função para manipular a opção escolhida pelo usuário
executarOpcao(Dados, '0') :-
    writeln('\nA água esquece o nome dos afogados...'),
    true.

executarOpcao(Dados, '1') :-
    cadastrarJogador(Dados, NovosDados),
    menu(NovosDados).

executarOpcao(Dados, '2') :-
    prepararJogo(Dados, 10).