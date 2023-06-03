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
    writeln('\n ● Digite 0 para sair \n\n'),
    ler_opcao(Op),
    executarOpcao(Dados, Op).

ler_opcao(Op):-
    writeln("→ Opção:"),
    read(Op).

% função para manipular a opção escolhida pelo usuário
executarOpcao(Dados, 0) :-
    writeln('\nA água esquece o nome dos afogados...'),
    true.

executarOpcao(Dados, 1) :-
    cadastrarJogador(Dados, NovosDados),
    menu(NovosDados).

executarOpcao(Dados, 2) :-
    prepararJogo(Dados).

executarOpcao(Dados, 4) :-
    exibirConteudoArquivo('historia.txt'),
    writeln("Pressione qualquer numero para voltar ao menu"),
    read(_),
    menu(Dados).

prepararJogo(Dados) :-
    shell(clear), % limpa a tela
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n ● Digite 1 para jogar com a máquina'),
    writeln('\n ● Digite 2 para jogar com dois jogadores'),
    writeln('\n ● Digite 3 para redimensionar o tabuleiro'),
    writeln('\n ● Digite 0 para voltar ao menu\n\n'),
    ler_opcao(Op),
    pegaTamanhoTabuleiro(Op, TamTab),
    executarOpcaoJogo(Dados, Op, TamTab).

executarOpcaoJogo(Dados, 0, _) :-
    prepararJogo(Dados).

executarOpcaoJogo(Dados, 1, TamTab) :-
    doWhile(true, Dados, TamTab).

executarOpcaoJogo(Dados, 2, TamTab) :-
    prepararJogo(Dados, TamTab).


pegaTamanhoTabuleiro(3, Tam) :-
    shell(clear),
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n • Qual o novo tamanho do tabuleiro? (Digite somente um valor ex: 10)\n\n'),
    read_string(user_input, "\n", "\r", _, Linha),
    ler_opcao(Tam).

pegaTamanhoTabuleiro(_, 10).

executarOpcaoJogo(Dados, Opcao, TamTab) :-
    Opcao \= 3,
    writeln("Opção inválida... Pressione ENTER para tentar novamente!\n"),
    read(_),
    menu(Dados, TamTab).

doWhile(true, Dados, TamTab):-
    shell(clear),
    montaTabuleiro("", Tabuleiro_Jogador, TamTab),
    preparaTabParaPrint(Tabuleiro_Jogador, 0, Tab_R),
	write(Tab_R),
	montaTabuleiro("", Tabuleiro_Jogador_Ve_Bot, TamTab),
	montaTabuleiro("B", Tabuleiro_Bot, TamTab),
	montaTabuleiro("", Tabuleiro_Bot_Ve_Jogador, TamTab).


doWhile(false, Dados, TamTab):- menu(Dados).

montaTabuleiro("", Tabuleiro_Jogador, TamTab):-
	montaMatriz(_, 0, Tabuleiro_Jogador, TamTab).

montaTabuleiro("B", Tabuleiro_Bot, TamTab):-
	montaMatriz(_, 0, Tab, TamTab).
	% montaTabuleiroBotInteiro(Tab, Tabuleiro_Bot). % Falta fazer essa função

montaMatriz(R, TamTab, R, TamTab).
montaMatriz(LinhaEntrada, I, R, TamTab):-
	I >= 0,
	I < TamTab,
	I1 is I + 1,
	montaListaTab(_, 0, Linha, TamTab),
	append(LinhaEntrada, [Linha], MatrizFinal),
	montaMatriz(MatrizFinal, I1, R, TamTab).

montaListaTab(R, TamTab, R, TamTab).
montaListaTab(K, J, R, TamTab):-
	J >= 0,
	J < TamTab,
	J1 is J + 1,
	append(K, [~], LinhaFinal),
	montaListaTab(LinhaFinal, J1, R, TamTab).










% Funções so pra ve o tabuleiro, não usem tem que mudar ainda
preparaTabParaPrint([], _, "").
preparaTabParaPrint([[]], _, "").
preparaTabParaPrint(Tab, 0, R):-
	preparaTabParaPrint(Tab, 1, R1),
	string_concat("    1 2 3 4 5 6 7 8 9 10\n", R1, R).
preparaTabParaPrint([H|_], 10, R):-
	insereEspacos(H, "", K),
	string_concat("10 ", K, R1),
	string_concat(R1, "\n", R).
preparaTabParaPrint([H|T], I, R):-
	I < 10,
	string_concat(" ", I, R1),
	string_concat(R1, " ", R2),
	insereEspacos(H, "", K),
	string_concat(R2, K, R3),
	string_concat(R3, "\n", R4),
	NovoI is I + 1,
	preparaTabParaPrint(T, NovoI, R5),
	string_concat(R4, R5, R).

insereEspacos([L|[]], K, R):-
	string_concat(K, L, R).
insereEspacos([T0, T1 | []], K, R):-
	string_concat(K, " ", R1),
	string_concat(R1, T0, R2),
	string_concat(R2, " ", R3),
	string_concat(R3, T1, R4),
	R = R4.
insereEspacos([T0 | T1], K, R):-
	string_concat(K, " ", R1),
	string_concat(R1, T0, R2),
	insereEspacos(T1, R2, R).