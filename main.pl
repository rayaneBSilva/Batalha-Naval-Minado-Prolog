% definição dos tipos de dados
:- dynamic jogador/2. % dynamic para permitir a modificação em tempo de execução
:- dynamic tabuleiro/1.

% função que inicia o programa
main :-
    exibirConteudoArquivo('introducao.txt'),
    sleep(1), % aguarda 5 segundos
    menu([]).

% ---------- INTRODUÇÃO E HISTÓRIA ----------
:- dynamic(jogador/1).
:- dynamic(tabuleiro/1).

% Função que exibe o conteúdo de um arquivo
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
    write('→ Opção:'),nl,
    read(Op).
    
% função para manipular a opção escolhida pelo usuário
executarOpcao(Dados, 0) :-
    writeln('\nA água esquece o nome dos afogados...'),
    true.

executarOpcao(Dados, 1) :-
    cadastrarJogador(Dados, NovosDados),
    menu(NovosDados).

executarOpcao(Dados, 2) :-
    prepararJogo(Dados, 10).

executarOpcao(Dados, 4) :-
    exibirConteudoArquivoLentamente('historia.txt'),
    writeln("Pressione qualquer número para voltar ao menu."),
    read(_),
    menu(Dados).


% ---------- CADASTRA JOGADOR ----------

cadastrarJogador(Dados, NovosDados) :-
    shell(clear), % limpa a tela
    writeln('                  Cadastro de jogadores                  '),
    writeln('\nDigite um nome de usuário: '),
    read(Nome),
    (   existeJogador(Dados, Nome)
    ->  writeln('\nEsse nome já existe, escolha outro.'),
        % writeln('\nPressione <Enter> para continuar...'),
        get_char(_),
        sleep(3), % Pausa por 3 segundos
        cadastrarJogador(Dados, NovosDados)
    ;   append(Dados, [jogador(Nome, 0)], NovosDados), % Atualiza a lista de jogadores
        open('dados.txt', write, Stream), % Abre o arquivo para escrita
        salvarJogadores(Stream, NovosDados), % Salva a lista de jogadores no arquivo
        close(Stream),
        format('\nUsuário ~s cadastrado com sucesso!', [Nome]),
        flush_output,
        sleep(3), % Pausa por 3 segundos
        writeln('\nPressione <Enter> para continuar...'),
        get_char(_),
        menu(NovosDados)
    ).

% Função auxiliar para verificar se um jogador já existe
existeJogador([], _) :- false.
existeJogador([jogador(Nome, _) | _], Nome) :- true.
existeJogador([_ | Jogadores], Nome) :-
    existeJogador(Jogadores, Nome).

% Função auxiliar para salvar a lista de jogadores no arquivo
salvarJogadores(_, []).
salvarJogadores(Stream, [jogador(Nome, Pontuacao) | Jogadores]) :-
    format(Stream, 'jogador(~w, ~w).\n', [Nome, Pontuacao]),
    salvarJogadores(Stream, Jogadores).

% ---------- FIM ----------


exibirConteudoArquivoLentamente(NomeArquivo) :-
    phrase_from_file(conteudoArquivo(T), NomeArquivo),
    imprimirLentamente(T).

conteudoArquivo([]) --> [].
conteudoArquivo([C|Cs]) -->
    [C],
    conteudoArquivo(Cs).

imprimirLentamente([]).
imprimirLentamente([C|Cs]) :-
    put_char(C),
    sleep(0.1),  % Tempo de espera entre as letras (ajuste conforme necessário)
    flush_output,
    imprimirLentamente(Cs).

    
prepararJogo(Dados, TamTab) :-
    shell(clear), % limpa a tela
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n ● Digite 1 para jogar com a máquina'),
    writeln('\n ● Digite 2 para jogar com dois jogadores'),
    writeln('\n ● Digite 3 para redimensionar o tabuleiro'),
    writeln('\n ● Digite 0 para voltar ao menu\n\n'),
    ler_opcao(Op),
    executarOpcaoJogo(Dados, Op, TamTab).

executarOpcaoJogo(Dados, 0, _) :-
    menu.
    

executarOpcaoJogo(Dados, 1, TamTab) :-
   doWhile(true, Dados, TamTab).

executarOpcaoJogo(Dados, 2, TamTab) :-
    prepararJogo(Dados, TamTab).

executarOpcaoJogo(Dados, 3, _) :-
    pegaTamanhoTabuleiro(3, TamTab),
    prepararJogo(Dados, TamTab).


pegaTamanhoTabuleiro(3, Tam) :-
    shell(clear),
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n • Qual o novo tamanho do tabuleiro? (Digite somente um valor ex: 10)\n\n'),
    ler_opcao(Tam).

pegaTamanhoTabuleiro(_, 10).

executarOpcaoJogo(Dados, Opcao, TamTab) :-
    Opcao \= 3,
    writeln('Opção inválida... Pressione ENTER para tentar novamente!\n'),
    read(_),
    menu(Dados, TamTab).

doWhile(true, Dados, TamTab):-
    shell(clear),
    montaTabuleiros(Tabuleiro_Jogador, Tabuleiro_Jogador_Ve_Bot, Tabuleiro_Bot, Tabuleiro_Bot_Ve_Jogador, TamTab),
    montaTabuleiroJogador(Tabuleiro_Jogador, TamTab, Tabuleiro_Jogador_Final),
    
    shell(clear),
    writeln('Tabuleiro do Jogador: '),
    preparaTabParaPrint(Tabuleiro_Jogador_Ve_Bot, 0, Tab_R),
	  write(Tab_R),
    writeln('Tabuleiro do Bot: '),
    preparaTabParaPrint(Tabuleiro_Bot_Ve_Jogador,0, Tab_bot_R),
    write(Tab_bot_R).
   


doWhile(false, Dados, TamTab):- menu(Dados).

% Relação que monta os tabuleiros
montaTabuleiros(Tabuleiro_Jogador, Tabuleiro_Jogador_Ve_Bot, Tabuleiro_Bot, Tabuleiro_Bot_Ve_Jogador, TamTab) :- 
  montaTabuleiro('', Tabuleiro_Jogador, TamTab),
	montaTabuleiro('', Tabuleiro_Jogador_Ve_Bot, TamTab),
	montaTabuleiro('B', Tabuleiro_Bot, TamTab),
	montaTabuleiro('', Tabuleiro_Bot_Ve_Jogador, TamTab).

montaTabuleiro('', Tabuleiro_Jogador, TamTab):-
	montaMatriz(_, 0, Tabuleiro_Jogador, TamTab).

montaTabuleiro('B', Tabuleiro_Bot, TamTab):-
	montaMatriz(_, 0, Tab, TamTab),
    Quantidade_de_navios is (TamTab // 2),
    montaTabuleiroBotInteiro(Tab, Tabuleiro_Bot, Quantidade_de_navios, TamTab). 

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

montaTabuleiroBotInteiro(Tab, Tab_BF, 2, TamTab):- montaTabuleiroBot(Tab, 2, Tab_BF, TamTab).
montaTabuleiroBotInteiro(Tab, Tab_BF, Quantidade_de_navios, TamTab):-
    Quantidade_de_navios > 1,
    montaTabuleiroBot(Tab, Quantidade_de_navios, Tab_B, TamTab),
    Quantidade_de_navios_Atua is Quantidade_de_navios - 1, 
    montaTabuleiroBotInteiro(Tab_B, Tab_BF, Quantidade_de_navios_Atua, TamTab).


montaTabuleiroBot(Tab, TamNavio, R, TamTab):-
    TamTabAux is TamTab + 1,
	random(1, TamTabAux, X),
	random(1, TamTabAux, Y),
	random(0, 2, Orient_I),
    
	verificaSeCabeNavio(Tab, X, Y, TamNavio, TamTab, Orient_I, Resultado),

    
	Resultado -> (
		posicionaNavios(Tab, X, Y, TamNavio, TamTab, Orient_I, R) % Falta fazer essa função, tem que pensar se vai precisar do tamanho do  navio para ela
	);
	montaTabuleiroBot(Tab, TamNavio, R, TamTab).


verificaSeCabeNavio(Tab, X, Y, TamNavio, TamTab, 0, Resultado):-
	Y_Aux is Y + TamNavio - 1,
	Y_Aux =< TamTab -> verificaTemNavioHorizontal(Tab, X, Y, TamNavio, Resultado);
	                    Resultado = false.

verificaSeCabeNavio(Tab, X, Y, TamNavio, TamTab, 1, Resultado):-
	X_Aux is X + TamNavio - 1,
	X_Aux =< TamTab -> verificaTemNavioVertical(Tab, X, Y, TamNavio, Resultado);
	                   Resultado = false.


verificaTemNavioHorizontal(Tab, X, Y, TamNavio, TabResul):-
    nth1(X, Tab, Elemento),
    NumDrops is Y - 1,
    drop(NumDrops, Elemento, ResulTabDrop),
    take(TamNavio, ResulTabDrop, ResulTabTake),
    temNavio(ResulTabTake, R1),
    (R1 -> TabResul = false; TabResul = true). % Se não pegar usar a função notBool


verificaTemNavioVertical(Tab, X, Y, TamNavio, TabResul):-
    transpose(Tab, TabTranspose),
	verificaTemNavioHorizontal(TabTranspose, Y, X, TamNavio, TabResul).


% Implementando a função drop de haskell em prolog
drop(0, UltimoEle,UltimoEle) :- !.
drop(N,[_|Tail],UltimoEle) :-
  N > 0,
  N1 is N - 1,
  drop(N1,Tail,UltimoEle).

% Implementando a função take de haskell em prolog
take(0, _, []) :- !.
take(N, [H|TailA], [H|TailB]) :-
  N > 0,
  N2 is N - 1,
  take(N2, TailA, TailB).

temNavio([], false).
temNavio(['#'|_], true).
temNavio(['#'|_], true).
temNavio([~|T], R):- temNavio(T, R).
temNavio([x|T], R):- temNavio(T, R).
temNavio([o|T], R):- temNavio(T, R).


transpose([[]|_], []).
transpose(Tab, [Linha|Linha2]) :- transpose_coluna(Tab, Linha, TabResul),
                                 transpose(TabResul, Linha2).
transpose_coluna([], [], []).
transpose_coluna([[H|T]|Linha2], [H|Hs], [T|Ts]) :- transpose_coluna(Linha2, Hs, Ts).

% Função que ira posicionar os navios na horizontal
posicionaNavios(Tab, X, Y, TamNavio, TamTab, 0, R):- posicionaNaviosHorizontal(Tab, X, Y, TamNavio, TamTab, R).

% Função que ira posicionar os navios na vertical
posicionaNavios(Tab, X, Y, TamNavio, TamTab, 1, R):- posicionaNaviosVertical(Tab, X, Y, TamNavio, TamTab, R).


posicionaNaviosVertical(Tab, X, Y, TamNavio, TamTab, Resultado):-
	transpose(Tab, TabTransp),
	posicionaNaviosHorizontal(TabTransp, Y, X, TamNavio, TamTab, TabResul),
	transpose(TabResul, Resultado).


posicionaNaviosHorizontal(TabTransp, X, Y, TamNavio, TamTab, Resultado):- 
  LinhaInserir is X - 1,
  PosInserir is Y - 1,
  nth0(LinhaInserir, TabTransp, Linha),
  drop(PosInserir, Linha, LinhaDrop),
  take(TamNavio, LinhaDrop, LinhaDropTake),
  temNavio(LinhaDropTake, TabResul),
  negateBool(TabResul, NotTabResul),
  (NotTabResul -> adicionandoNavioHorizontal(TabTransp, TamNavio, TamTab, LinhaInserir, PosInserir, Resultado);
  Resultado = []).
  
  
  
  
% Função equivalente a remonta navios
adicionandoNavioHorizontal([Head|[]], TamNavio, TamTab, 0, Posicao_Y, Resultado):- 
	LimiteMin = Posicao_Y,
	LimiteMax is Posicao_Y + TamNavio - 1, 
	montaListaComNavio(Head, 0, LimiteMin, LimiteMax, [], true, Resultado).% montaListaComNavio(Head, 0, TamTab, LimiteMin, LimiteMax, [], true, Resultado).

adicionandoNavioHorizontal([H|T], TamNavio, TamTab, 0, Posicao_Y, Resultado):-
	LimiteMin = Posicao_Y,
	LimiteMax is Posicao_Y + TamNavio - 1, 
	montaListaComNavio(H, 0, TamTab, LimiteMin, LimiteMax, [], R),                   
	adicionandoNavioHorizontal(T, TamNavio, TamTab, -1, Posicao_Y, R2),
	append([R], R2, Resultado).

adicionandoNavioHorizontal([H|[]], _, _, _, _, [H]). 

adicionandoNavioHorizontal([H|T], TamNavio, TamTab, Posicao_X, Posicao_Y, Resultado):-
  Pos_X_Aux is Posicao_X - 1,
	adicionandoNavioHorizontal(T, TamNavio, TamTab, Pos_X_Aux, Posicao_Y, Resul),
	append([H], Resul, Resultado).

montaTabuleiroJogador(Tab, TamTab, TabResul):-
    TamNavios is (TamTab // 2),
    posicionaNaviosTabJogador(Tab, TamNavios, TamTab, TabResul).


posicionaNaviosTabJogador(Tab, 2, TamTab, TabResul) :- posicionaNaviosJogador(Tab, 2, TamTab, TabResul).
posicionaNaviosTabJogador(Tab, TamNavio, TamTab, TabResul):-
    TamNavio > 1,
    TamNaviosAux is TamNavio - 1,
    posicionaNaviosJogador(Tab, TamNavio, TamTab, TabR),
    posicionaNaviosTabJogador(TabR, TamNaviosAux, TamTab, TabResul).


posicionaNaviosJogador(Tab, TamNavio, TamTab, TabResultado):-
  shell(clear),
  write('Seu tabuleiro\n'),
	preparaTabParaPrint(Tab, 0, Tab_R),
	write(Tab_R),
 
  write('Insira as posicoes X de 1 a '), write(TamTab), write(' e Y de 1 a '), write(TamTab), write(' e a ORIENTACAO (H ou V) para posicionar seu navio.\n'),
	write('Tamanho do navio: '), write(TamNavio),

	write('\nValor de X: '),
  read(X),
	write('\nValor de Y: '),
	read(Y),
  write('\nOrientação: '),
  read_string(user_input, ".", "\n", _, Ori),


	verificaEntrada(X, TamTab, R1),
	verificaEntrada(Y, TamTab, R2),
	verificaOrientacao(Ori, R3),
	verificaLimites(Tab, Ori, X, Y, TamNavio, TamTab, RLimite),

	((R1, R2, R3, RLimite) -> (insereNavioTab(Tab, X, Y, Ori, TamNavio, TamTab, TabResultado));
	           write('\nInformacoes invalidas, tente novamente.\n'),
	           sleep(3),
	           posicionaNaviosJogador(Tab, TamNavio, TamTab, TabResultado)).


verificaEntrada(X, TamTab, R):-
	(X < 1; X > TamTab) -> (
		write('O valor eh invalido, insira um valor entre 1 e '), write(TamTab), write('\n'),
		sleep(2.5),
		R = false, !
	); R = true.

verificaOrientacao("H", true):- !.
verificaOrientacao("V", true):- !.
verificaOrientacao(_, false):- 
  write('O valor digitado é invalido\n'),
	sleep(2.5).


verificaLimites(Tab, "H", X, Y, TamNavio, TamTab, R):-
	K is Y + TamNavio - 1,
	(K =< TamTab) -> (
		verificaTemNavioHorizontal(Tab, X, Y, TamNavio, R), !     
	);
	R = false.

verificaLimites(Tab, "V", X, Y, TamNavio, TamTab, R):-
	K is X + TamNavio - 1,
	(K =< TamTab) -> (
		verificaTemNavioVertical(Tab, X, Y, TamNavio, R), !
	);
	R = false.
 
 verificaLimites(_, _, _, _, _, _, false).


insereNavioTab(Tab, X, Y, Orientacao, TamNavio, TamTab, R):-
	posicionaFinal(Tab, X, Y, Orientacao, TamNavio, TamTab, R).

posicionaFinal(Tab, X, Y, "H", TamNavio, TamTab, R):-
	posicionaNaviosHorizontal(Tab, X, Y, TamNavio, TamTab, R).

posicionaFinal(Tab, X, Y, "V", TamNavio, TamTab, R):-
	posicionaNaviosVertical(Tab, X, Y, TamNavio, TamTab, R).



% Negação do Bool
negateBool(false, true).
negateBool(true, false).

% Funções so pra ve o tabuleiro
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


printaTabEMensagem(Tab, Mensagem):-
	write(Mensagem),
	preparaTabParaPrint(Tab, 0, Tab_R),
	write(Tab_R).




montaListaComNavio(_, 10, _, _, NovaLista, true, [NovaLista]).
montaListaComNavio(LEntrada, I, MinI, MaxI, LSaida, true, R):-
	I >= 0,
	I < 10,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, LEntrada, ElementoInteresse),
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaListaComNavio(LEntrada, NovoI, MinI, MaxI, NovoLSaida, true, R));
		(ElementoInteresse = #,
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaListaComNavio(LEntrada, NovoI, MinI, MaxI, NovoLSaida, true, R)).

montaListaComNavio(_, 10, _, _, NovaLista, NovaLista).
montaListaComNavio(LEntrada, I, MinI, MaxI, LSaida, R):-
	I >= 0,
	I < 10,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, LEntrada, ElementoInteresse),
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaListaComNavio(LEntrada, NovoI, MinI, MaxI, NovoLSaida, R));
		(ElementoInteresse = #,
		NovoI is I + 1,
		append(LSaida, [ElementoInteresse], NovoLSaida),
		montaListaComNavio(LEntrada, NovoI, MinI, MaxI, NovoLSaida, R)).