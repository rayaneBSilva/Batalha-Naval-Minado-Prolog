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
    (   existeJogador('dados.txt', Nome)
    ->  writeln('\nEsse nome já existe, escolha outro.'),
        % writeln('\nPressione <Enter> para continuar...'),
        get_char(_),
        sleep(3), % Pausa por 3 segundos
        menu(Dados) % Retorna ao menu principal sem modificar a lista de jogadores
    ;   append(Dados, [jogador(Nome, 0)], NovosDados), % Atualiza a lista de jogadores
        salvarJogadores('dados.txt', NovosDados), 
        format('\nUsuário ~s cadastrado com sucesso!', [Nome]),
        flush_output,
        sleep(3), % Pausa por 3 segundos
        writeln('\nPressione <Enter> para continuar...'),
        get_char(_),
        menu(NovosDados)
    ).


existeJogador(Arquivo, Nome) :-
    open(Arquivo, read, Stream), 
    existeJogadorAux(Stream, Nome),  
    close(Stream).
    
% Função auxiliar para verificar se um jogador já existe no arquivo
existeJogadorAux(Stream, Nome) :-
    \+ at_end_of_stream(Stream),             
    read(Stream, jogador(NomeArquivo, _)),    
    ( Nome = NomeArquivo,                    
      ! ;
      existeJogadorAux(Stream, Nome)       
    ).


% Função auxiliar para salvar a lista de jogadores no arquivo
salvarJogadores(_, []).
salvarJogadores(Arquivo, [jogador(Nome, Pontuacao) | Jogadores]) :-
    open(Arquivo, append, Stream), 
    format(Stream, 'jogador(~w, ~w).\n', [Nome, Pontuacao]),
    close(Stream),
    salvarJogadores(Arquivo, Jogadores).

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
    menu(Dados).
    

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
    writeln('Tabuleiro do Jogador: \n'),
    preparaTabParaPrint(Tabuleiro_Jogador_Final, 0, TamTab, Tab_R),
	write(Tab_R),
    writeln('Tabuleiro do Bot: \n'),
    preparaTabParaPrint(Tabuleiro_Bot,0, TamTab, Tab_bot_R),
    write(Tab_bot_R),

    iniciaJogoComMaquina(Tabuleiro_Jogador_Final, Tabuleiro_Jogador_Ve_Bot, Tabuleiro_Bot, Tabuleiro_Bot_Ve_Jogador, TamTab, Dados).


iniciaJogoComMaquina(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, TamTab, Dados):-
    contaNavios(Tab_J, NumNavios_J),
	contaNavios(Tab_B, NumNavios_B),

	verificaFinalizacaoPartida(NumNavios_J, NumNavios_B, Continue),

    (Continue -> 
        shell(clear),
        writeln('Esse é o tabuleiro que o Jogador vai jogar: \n'), % Mudar Frase
        preparaTabParaPrint(Tab_J_Ve_B, 0, TamTab, Tab_J_R),
        write(Tab_J_R),
        writeln('\nEsse é o tabuleiro que o Bot vai jogar: \n'),
        preparaTabParaPrint(Tab_B_Ve_J, 0, TamTab, Tab_B_R),
        write(Tab_B_R),
        write('Numero de navios restantes do jogador: '), write(NumNavios_J), write('\n'),
	    write('Numero de navios restantes do bot: '), write(NumNavios_B), write('\n\n'),
        
        disparaNoTabuleiroBot(Tab_B, Tab_J_Ve_B, TamTab, Tab_BF, Tab_J_Ve_BF),
        write('\nVez do bot...\n'),
		sleep(0.9),
		disparaNoTabuleiroJogador(Tab_J, Tab_B_Ve_J, TamTab, Tab_JF, Tab_B_Ve_JF),
        iniciaJogoComMaquina(Tab_JF, Tab_J_Ve_BF, Tab_BF, Tab_B_Ve_JF, TamTab, Dados); 
        
        write('Você quer jogar novamente? [1 para sim, outro número para sair]'),
        ler_opcao(Op),
        (Op =:= 1 -> doWhile(true, Dados, TamTab); menu(Dados))  % VERIFICAR SE É NECESSARIO A VARIAVEL DADOS E O TRUE
        ).



contaNavios([H|[]], NumNaviosFinal):- contaNaviosLinha(H, NumNaviosFinal).
contaNavios([H|T], NumNaviosFinal):-
    contaNaviosLinha(H, NumNavios),
	contaNavios(T, NumNavios2),
	NumNaviosFinal is NumNavios + NumNavios2.

contaNaviosLinha([~|[]], 0).
contaNaviosLinha([x|[]], 0).
contaNaviosLinha([o|[]], 0).
contaNaviosLinha([#|[]], 1).
contaNaviosLinha([~|T], NumNaviosFinal):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosFinal is 0 + NumNavios.
contaNaviosLinha([x|T], NumNaviosFinal):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosFinal is 0 + NumNavios.
contaNaviosLinha([o|T], NumNaviosFinal):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosFinal is 0 + NumNavios.
contaNaviosLinha([#|T], NumNaviosFinal):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosFinal is 1 + NumNavios.

verificaFinalizacaoPartida(0, K, false):-
	K \= 0,
    writeln('Que pena você perdeu! Seus navios foram para o fundo do mar!').

verificaFinalizacaoPartida(_, 0, false):-
    writeln('Você é um verdadeiro almirante! Parabéns pela vitória na batalha naval.').

verificaFinalizacaoPartida(A, B, true):- A =\= 0, B =\= 0.

disparaNoTabuleiroBot(Tab_B, Tab_J_Ve_B, TamTab, Tab_Bot_Final, Tab_Jogador_Ve_Bot_Final):-
    write('Sua vez de disparar.\n'),
    write('Insira as posicoes X de 1 a '), write(TamTab), write(' e Y de 1 a '), write(TamTab), write('\n'),

	write('\nValor de X: '),
    read(X),
    write('\nValor de Y: '),
    read(Y),

	verificaEntrada(X, TamTab, R1),
	verificaEntrada(Y, TamTab, R2),

	verificaSeJaFoiDisparadoNoBot(Tab_J_Ve_B, X, Y, Resul),

	(R1, R2, Resul) -> (
		selecionaSimboloNavio(Tab_B, X, Y, Simbolo),
		posicionaSimboloNoNavio(Tab_B, Tab_J_Ve_B, X, Y, Simbolo, TamTab, Tab_Bot_Final, Tab_Jogador_Ve_Bot_Final)
	);
	write('Posicao invalida, digite uma nova posicao valida.\n'),
	disparaNoTabuleiroBot(Tab_B, Tab_J_Ve_B, TamTab, Tab_Bot_Final, Tab_Jogador_Ve_Bot_Final).


% VERIFICA SE ESSAS FUNÇÕES FUNCIONAM
verificaSeJaFoiDisparadoNoBot(Tab, X, Y, R):-
    nth1(X, Tab, Linha),
	nth1(Y, Linha, Elemento),
	foiDisparadoNoBot(Elemento, R).


% VERIFICA SE ESSAS FUNÇÕES FUNCIONAM
foiDisparadoNoBot(x, false).
foiDisparadoNoBot(o, false).
foiDisparadoNoBot(#, true).
foiDisparadoNoBot(~, true).
foiDisparadoNoBot(E, R):- E \= x, E \= o, R = true.


selecionaSimboloNavio(Tab, X, Y, SimboloFinal):-
	nth1(X, Tab, Linha),
	nth1(Y, Linha, Simbolo),
	simboloRetornado(Simbolo, SimboloFinal).

simboloRetornado(#, x).
simboloRetornado(~, o).


posicionaSimboloNoNavio(Tab_B, Tab_J_Ve_B, X, Y, x, TamTab, Tab_Bot_Final, Tab_J_Ve_Bot_Final):-
	write('Voce acertou um navio!\n'),
	posicionaSimbolo(Tab_B, X, Y, x, TamTab, Tab_Bot_Final),
	posicionaSimbolo(Tab_J_Ve_B, X, Y, x, TamTab, Tab_J_Ve_Bot_Final).


posicionaSimboloNoNavio(Tab_B, Tab_J_Ve_B, X, Y, o, TamTab, Tab_B, Tab_J_Ve_Bot_Final):-
	write('Voce acertou na agua!\n'),
	posicionaSimbolo(Tab_J_Ve_B, X, Y, o, TamTab, Tab_J_Ve_Bot_Final).


posicionaSimbolo(Tab, X, Y, Simbolo, TamTab, Tab_Final):-
	X_Aux is X - 1,
	Y_Aux is Y - 1,
  colocaSimboloNoTabuleiro(Tab, 1, X_Aux, Y_Aux, Simbolo, TamTab, Tab_Final).

colocaSimboloNoTabuleiro([H|[]], 1, 0, Y, Elemento, TamTab, R):- 
  montaListaComSimbolo(H, 0, TamTab, Y, Y, [], Elemento, true, R).
    
colocaSimboloNoTabuleiro([H|T], 1, 0, Y, Elemento, TamTab, R):-
  montaListaComSimbolo(H, 0, TamTab, Y, Y, [], Elemento, false, R1),
  colocaSimboloNoTabuleiro(T, 1, -1, Y, Elemento, TamTab, R2),
  append([R1], R2, R).
    
colocaSimboloNoTabuleiro([H|[]], _, X, _, _, _, [H]):- X =\= 0.
    
colocaSimboloNoTabuleiro([H|T], 1, X, Y, Elemento, TamTab, R):-
  X =\= 0,
  Novo_X is X - 1,
  colocaSimboloNoTabuleiro(T, 1, Novo_X, Y, Elemento, TamTab, R1),
  append([H], R1, R).
    
montaListaComSimbolo(_, TamTab, TamTab, _, _, NovaLista, _, true, R):- R = [NovaLista].
montaListaComSimbolo(_, TamTab, TamTab, _, _, NovaLista, _, false, R):- R = NovaLista.
montaListaComSimbolo(Lista, I, TamTab, MinI, MaxI, ListaSaida, Elemento, Boolean, R):-
  I >= 0,
  I < TamTab,
  ((I < MinI); (I > MaxI)) ->
  (nth0(I, Lista, ElementoAdicionado),
  NovoI is I + 1,
  append(ListaSaida, [ElementoAdicionado], NovoListaSaida),
  montaListaComSimbolo(Lista, NovoI, TamTab, MinI, MaxI, NovoListaSaida, Elemento, Boolean, R));
  (ElementoAdicionado = Elemento,
  NovoI is I + 1,
  append(ListaSaida, [ElementoAdicionado], NovoListaSaida),
  montaListaComSimbolo(Lista, NovoI, TamTab, MinI, MaxI, NovoListaSaida, Elemento, Boolean, R)).










disparaNoTabuleiroJogador(Tab_J, Tab_B_Ve_J, TamTab, Tab_JF, Tab_B_Ve_JF):-
    Tamanho is TamTab + 1,
	random(1, Tamanho, X),
	random(1, Tamanho, Y),

	verificaJaDisparadoAoJogador(Tab_B_Ve_J, X, Y, Tamanho, Resposta),

	Resposta -> (
		selecionaSimboloNavio(Tab_J, X, Y, Simbolo),
		posicionaSimboloNoNavio(Tab_J, Tab_B_Ve_J, X, Y, Simbolo, TamTab, Tab_JF, Tab_B_Ve_JF)
	);
	disparaNoTabuleiroJogador(Tab_J, Tab_B_Ve_J, TamTab, Tab_JF, Tab_B_Ve_JF).


verificaJaDisparadoAoJogador(Tab, X, Y, Tamanho, R):-
	(X > 0, X < Tamanho, Y > 0, Y < Tamanho) -> (
		nth1(X, Tab, Linha),
		nth1(Y, Linha, Elemento),
		ehDisparoAoJogador(Elemento, R)
	);
	R = false.

% AJEITAR
ehDisparoAoJogador(x, false).
ehDisparoAoJogador(o, false).
ehDisparoAoJogador(#, true).
ehDisparoAoJogador(~, true).





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


posicionaNaviosHorizontal(Tab, X, Y, TamNavio, TamTab, Resultado):- 
  X2 is X - 1,
  Y2 is Y - 1,
  nth0(X2, Tab, Linha),
  drop(Y2, Linha, LinhaDrop),
  take(TamNavio, LinhaDrop, LinhaDropTake),
  temNavio(LinhaDropTake, TabResul),
  negateBool(TabResul, NotTabResul),
  (NotTabResul -> adicionandoNavioHorizontal(Tab, TamNavio, TamTab, X2, Y2, Resultado);
  Resultado = []).
  
  
  
  
% Função equivalente a remonta navios
adicionandoNavioHorizontal([Head|[]], TamNavio, TamTab, 0, Posicao_Y, Resultado):- 
	LimiteMin = Posicao_Y,
	LimiteMax is Posicao_Y + TamNavio - 1, 
	montaListaComNavio(Head, 0, TamTab, LimiteMin, LimiteMax, [], true, Resultado).% montaListaComNavio(Head, 0, TamTab, LimiteMin, LimiteMax, [], true, Resultado).

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
    TamNavio > 2,
    TamNaviosAux is TamNavio - 1,
    posicionaNaviosJogador(Tab, TamNavio, TamTab, TabR),
    posicionaNaviosTabJogador(TabR, TamNaviosAux, TamTab, TabResul).


posicionaNaviosJogador(Tab, TamNavio, TamTab, TabResultado):-
  shell(clear),
  writeln('          Seu tabuleiro\n'),
  preparaTabParaPrint(Tab, 0, TamTab, Tab_R),
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


insereNavioTab(Tab, X, Y, "H", TamNavio, TamTab, R):-
	posicionaNaviosHorizontal(Tab, X, Y, TamNavio, TamTab, R).

insereNavioTab(Tab, X, Y, "V", TamNavio, TamTab, R):-
	posicionaNaviosVertical(Tab, X, Y, TamNavio, TamTab, R).


% Negação do Bool
negateBool(false, true).
negateBool(true, false).



montaListaComNavio(_, TamTab, TamTab, _, _, NovaLista, true, [NovaLista]).
montaListaComNavio(Lista, I, TamTab, MinI, MaxI, ListaResul, true, R):-
	I >= 0,
	I < TamTab,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, Lista, Elemento),
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComNavio(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, true, R));
		(Elemento = #,
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComNavio(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, true, R)).

montaListaComNavio(_, TamTab, TamTab, _, _, NovaLista, NovaLista).
montaListaComNavio(Lista, I, TamTab, MinI, MaxI, ListaResul, R):-
	I >= 0,
	I < TamTab,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, Lista, Elemento),
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComNavio(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, R));
		(Elemento = #,
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComNavio(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, R)).













% Funções so pra ve o tabuleiro, não usem tem que mudar ainda
preparaTabParaPrint([], _, _, "").

preparaTabParaPrint([[]], _, _, "").

preparaTabParaPrint(Tab, 0, TamTab, R):-
	preparaTabParaPrint(Tab, 1, TamTab, R1),
    criaStringDeNumero(TamTab, StrResul),
    string_concat(StrResul, "\n", StrResul2),
	string_concat(StrResul2, R1, R).

preparaTabParaPrint([H|_], TamTab, TamTab, R):-
	imprimiListaComEspaco(H, "", H2),
	insereEspacos([H2], " ", K),
    string_concat(TamTab, " ", R1),
	string_concat(R1, K, R2),
	string_concat(R2, "\n", R).


preparaTabParaPrint([H|T], I, TamTab, R):-
	I < TamTab, I >= 10, 
	string_concat(I, " ", R2),
    imprimiListaComEspaco(H, "", H2),
	insereEspacos([H2], " ", K),
	string_concat(R2, K, R3),
	string_concat(R3, "\n", R4),
	NovoI is I + 1,
	preparaTabParaPrint(T, NovoI, TamTab, R5),
	string_concat(R4, R5, R).


preparaTabParaPrint([H|T], I, TamTab, R):-
	I < TamTab, I < 10,
	string_concat(" ", I, R1),
	string_concat(R1, " ", R2),
    imprimiListaComEspaco(H, "", H2),
	insereEspacos([H2], " ", K),
	string_concat(R2, K, R3),
	string_concat(R3, "\n", R4),
	NovoI is I + 1,
	preparaTabParaPrint(T, NovoI, TamTab, R5),
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


criaStringDeNumero(TamTab, StrResul):-
    montaString("     ", 1, TamTab, StrResul).

montaString(StrInicial, TamTab, TamTab, StrResul):-
    string_concat(TamTab, "   ", Str_Aux),
    string_concat(StrInicial, Str_Aux, StrResul).

montaString(StrInicial, Indice, TamTab, StrResul):-
    Indice < TamTab, Indice < 10,
    string_concat(Indice, "   ", Str_Aux),
    string_concat(StrInicial, Str_Aux, R1),
    NovoIndice is Indice + 1,
    montaString(R1, NovoIndice, TamTab, StrResul).

montaString(StrInicial, Indice, TamTab, StrResul):-
    Indice < TamTab, Indice >= 10,
    string_concat(Indice, "  ", Str_Aux),
    string_concat(StrInicial, Str_Aux, R1),
    NovoIndice is Indice + 1,
    montaString(R1, NovoIndice, TamTab, StrResul).

imprimiListaComEspaco([], R, R).
imprimiListaComEspaco([H|T], StrInicio, R):-
    string_concat(StrInicio, " ", R1),
    string_concat(R1, H, R2),
    string_concat(R2, "  ", StrAux),
    imprimiListaComEspaco(T, StrAux, R).