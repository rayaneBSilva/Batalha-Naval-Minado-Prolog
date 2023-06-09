% definição dos tipos de dados
:- dynamic jogador/2. % dynamic para permitir a modificação em tempo de execução
:- dynamic tabuleiro/1.

% :- use_module(library(string)).

% regra que inicia o programa
main :-
    exibirConteudoArquivo('introducao.txt'),
    sleep(1), % aguarda 5 segundos
    menu([]).

% ---------- INTRODUÇÃO E HISTÓRIA ----------
:- dynamic(jogador/1).
:- dynamic(tabuleiro/1).

% regra que exibe o conteúdo de um arquivo
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

% regra que exibe o Menu
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

    
% regras para manipular a opção escolhida pelo usuário
executarOpcao(_, 0) :-
    writeln('\nA água esquece o nome dos afogados...'),
    true.

executarOpcao(Dados, 1) :-
    cadastrarJogador(Dados, NovosDados),
    menu(NovosDados).

executarOpcao(Dados, 2) :-
    prepararJogo(Dados, 10).

executarOpcao(Dados,3) :-
    shell(clear),
    writeln("-------------- RANKING DOS JOGADORES -----------"),
    lerArquivo('dados.txt'),
    writeln("Pressione qualquer número para voltar ao menu."),
    read(_),
    menu(Dados).

executarOpcao(Dados, 4) :-
    exibirConteudoArquivoLentamente('historia.txt'),
    writeln("Pressione qualquer número para voltar ao menu."),
    read(_),
    menu(Dados).

executarOpcao(Dados, _) :-
    Opcao \= 0, Opcao \= 1, Opcao \= 2, Opcao \= 3, Opcao \= 4,
    writeln('Opção inválida... Pressione ENTER para tentar novamente!\n'),
    read(_),
    menu(Dados).

% ---------- CADASTRA JOGADOR ----------
% regra responsável pelo cadastro de jogadores
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


% regra para verificar se um jogador existe
existeJogador(Arquivo, Nome) :-
    open(Arquivo, read, Stream), 
    existeJogadorAux(Stream, Nome),  
    close(Stream).
    

% regra auxiliar para verificar se um jogador já existe no arquivo
existeJogadorAux(Stream, Nome) :-
    \+ at_end_of_stream(Stream),             
    read(Stream, jogador(NomeArquivo, _)),    
    ( Nome = NomeArquivo,                    
      ! ;
      existeJogadorAux(Stream, Nome)       
    ).


% regra auxiliar para salvar a lista de jogadores no arquivo
salvarJogadores(_, []).
salvarJogadores(Arquivo, [jogador(Nome, Pontuacao) | Jogadores]) :-
    open(Arquivo, append, Stream), 
    format(Stream, 'jogador(~w, ~w).\n', [Nome, Pontuacao]),
    close(Stream),
    salvarJogadores(Arquivo, Jogadores).


% regra para imprimir lentamente a introdução do jogo
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


% regra que prepara o segundo menu do jogo  
prepararJogo(Dados, TamTab) :-
    shell(clear), % limpa a tela
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n ● Digite 1 para jogar com a máquina'),
    writeln('\n ● Digite 2 para jogar com dois jogadores'),
    writeln('\n ● Digite 3 para redimensionar o tabuleiro'),
    writeln('\n ● Digite 0 para voltar ao menu\n\n'),
    ler_opcao(Op),
    executarOpcaoJogo(Dados, Op, TamTab).


% regra para manipular a opção escolhida pelo usuário do segundo menu
executarOpcaoJogo(Dados, 0, _) :-
    menu(Dados).
    
executarOpcaoJogo(Dados, 1, TamTab) :-
   doWhile(true, Dados, TamTab).

executarOpcaoJogo(Dados, 2, TamTab) :-
    doWhileJogoCom2(true, Dados, TamTab, DadosAtualizado).

executarOpcaoJogo(Dados, 3, _) :-
    pegaTamanhoTabuleiro(3, TamTab),
    prepararJogo(Dados, TamTab).

executarOpcaoJogo(Dados, _, _) :-
    Opcao \= 3, Opcao \= 0, Opcao \= 1, Opcao \= 2,
    writeln('Opção inválida... Pressione ENTER para tentar novamente!\n'),
    read(_),
    menu(Dados).


% regra que executa um loop do jogo com o bot
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

doWhile(false, Dados, _):- menu(Dados).


% regra que executa um loop do jogo com dois jogadores
doWhileJogoCom2(true, Dados, TamTab, NovosDados) :-
    shell('clear'),
    chamaJogador(Dados, '', "1", Jogador1),
    chamaJogador(Dados, Jogador1, "2", Jogador2),
    
    (
        (Jogador1 = "JogadorNaoExiste"; Jogador2 = "JogadorNaoExiste") ->
        doWhileJogoCom2(true, Dados, TamTab, NovosDados)
        ;
        (
            shell('clear'),
            montaTabuleirosDeDoisJogadores(Tabuleiro_Jogador1, Tabuleiro_Jogador1_Ve_Jog2, Tabuleiro_Jogador2, Tabuleiro_Jogador2_Ve_Jog1, TamTab),
            montaTabuleiroJogador(Tabuleiro_Jogador1, TamTab, Tabuleiro_Jogador1_Final),
            montaTabuleiroJogador(Tabuleiro_Jogador2, TamTab, Tabuleiro_Jogador2_Final),
            
            shell('clear'),
            writeln('Tabuleiro do Jogador 1: \n'),
            preparaTabParaPrint(Tabuleiro_Jogador1_Final, 0, TamTab, Tab_R),
            write(Tab_R),
            writeln('Tabuleiro do Jogador 2: \n'),
            preparaTabParaPrint(Tabuleiro_Jogador2_Final, 0, TamTab, Tab_R2),
            write(Tab_R2),
            
            iniciaJogoComJogadores(Tabuleiro_Jogador1_Final, Tabuleiro_Jogador1_Ve_Jog2, Tabuleiro_Jogador2_Final, Tabuleiro_Jogador2_Ve_Jog1, TamTab, Dados, NovosDados)
        )
    ).


% regra que pega o tamanho do tabuleiro escolhido pelo usuario
pegaTamanhoTabuleiro(3, Tam) :-
    shell(clear),
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n • Qual o novo tamanho do tabuleiro? (Digite somente um valor ex: 10)\n\n'),
    ler_opcao(Tam).

pegaTamanhoTabuleiro(_, 10).

% regra que monta todos os tabuleiros do jogador e do bot
montaTabuleiros(Tabuleiro_Jogador, Tabuleiro_Jogador_Ve_Bot, Tabuleiro_Bot, Tabuleiro_Bot_Ve_Jogador, TamTab) :- 
    montaTabuleiro('', Tabuleiro_Jogador, TamTab),
	montaTabuleiro('', Tabuleiro_Jogador_Ve_Bot, TamTab),
	montaTabuleiro('B', Tabuleiro_Bot, TamTab),
	montaTabuleiro('', Tabuleiro_Bot_Ve_Jogador, TamTab).

% regra que monta todos os tabuleiros dos dois jogadores
montaTabuleirosDeDoisJogadores(Tabuleiro_Jogador1, Tabuleiro_Jogador1_Ve_Jog2, Tabuleiro_Jogador2, Tabuleiro_Jogador2_Ve_Jog1, TamTab) :- 
    montaTabuleiro('', Tabuleiro_Jogador1, TamTab),
    montaTabuleiro('', Tabuleiro_Jogador1_Ve_Jog2, TamTab),
    montaTabuleiro('', Tabuleiro_Jogador2, TamTab),
    montaTabuleiro('', Tabuleiro_Jogador2_Ve_Jog1, TamTab).

% regra que irá chamar a relação de montar matriz do tabuleiro do jogador
montaTabuleiro('', Tabuleiro_Jogador, TamTab):-
	montaMatriz(_, 0, Tabuleiro_Jogador, TamTab).

% regra que irá chamar a relação de montar matriz do tabuleiro do bot
montaTabuleiro('B', Tabuleiro_Bot_Bombas, TamTab):-
	montaMatriz(_, 0, Tab, TamTab),
    Quantidade_de_navios is (TamTab // 2),
    montaTabuleiroBotInteiro(Tab, Tabuleiro_Bot, Quantidade_de_navios, TamTab),
    jogaBombas(Tabuleiro_Bot, TamTab // 5, TamTab, Tabuleiro_Bot_Bombas). 

% regra que irá montar a matriz
montaMatriz(R, TamTab, R, TamTab).
montaMatriz(LinhaEntrada, I, R, TamTab):-
	I >= 0,
	I < TamTab,
	I1 is I + 1,
	montaListaTab(_, 0, Linha, TamTab),
	append(LinhaEntrada, [Linha], MatrizFinal),
	montaMatriz(MatrizFinal, I1, R, TamTab).

% regra que irá montar as linhas do tabuleiro
montaListaTab(R, TamTab, R, TamTab).
montaListaTab(K, J, R, TamTab):-
	J >= 0,
	J < TamTab,
	J1 is J + 1,
	append(K, [~], LinhaFinal),
	montaListaTab(LinhaFinal, J1, R, TamTab).

% regra que irá montar o tabuleiro do bot já adicionando o navios
montaTabuleiroBotInteiro(Tab, Tab_BF, 2, TamTab):- montaTabuleiroBot(Tab, 2, Tab_BF, TamTab).
montaTabuleiroBotInteiro(Tab, Tab_BF, Quantidade_de_navios, TamTab):-
    Quantidade_de_navios > 1,
    montaTabuleiroBot(Tab, Quantidade_de_navios, Tab_B, TamTab),
    Quantidade_de_navios_Atua is Quantidade_de_navios - 1, 
    montaTabuleiroBotInteiro(Tab_B, Tab_BF, Quantidade_de_navios_Atua, TamTab).

% regra que irá adicionar os navios no tabuleiro do bot
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

% regra que irá verificar se cabe um determinado navio em uma posição (X e Y), com orientação horizontal no tabuleiro
verificaSeCabeNavio(Tab, X, Y, TamNavio, TamTab, 0, Resultado):-
	Y_Aux is Y + TamNavio - 1,
	Y_Aux =< TamTab -> verificaTemNavioHorizontal(Tab, X, Y, TamNavio, Resultado);
	                    Resultado = false.

% regra que irá verificar se cabe um determinado navio em uma posição (X e Y), com orientação vertical no tabuleiro
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
    (R1 -> TabResul = false; TabResul = true). 


verificaTemNavioVertical(Tab, X, Y, TamNavio, TabResul):-
    transpose(Tab, TabTranspose),
	verificaTemNavioHorizontal(TabTranspose, Y, X, TamNavio, TabResul).


% regra responsavel por iniciar a partida com o bot
iniciaJogoComMaquina(Tab_J, Tab_J_Ve_B, Tab_B, Tab_B_Ve_J, TamTab, Dados):-
    contaNavios(Tab_J, NumNavios_J),
	contaNavios(Tab_B, NumNavios_B),

	verificaFinalizacaoPartida(NumNavios_J, NumNavios_B, Continue),

    (Continue -> 
        shell(clear),
        writeln('Esse é o tabuleiro que o Jogador vai jogar: \n'), 
        preparaTabParaPrint(Tab_J_Ve_B, 0, TamTab, Tab_J_R),
        write(Tab_J_R),
        writeln('\nEsse é o tabuleiro que o Bot vai jogar: \n'),
        preparaTabParaPrint(Tab_B_Ve_J, 0, TamTab, Tab_B_R),
        write(Tab_B_R),
        write('Numero de navios restantes do jogador: '), write(NumNavios_J), write('\n'),
	    write('Numero de navios restantes do bot: '), write(NumNavios_B), write('\n\n'),
        
        disparaNoTabuleiroBot(Tab_B, Tab_J_Ve_B, Tab_J, Tab_B_Ve_J, TamTab, Tab_BF, Tab_J_Ve_BF, Tab_J_Final, Tab_B_Ve_J_Final),

        contaNavios(Tab_J_Final, NumNavios_J2),
	    contaNavios(Tab_BF, NumNavios_B2),
        verificaFinalizacaoPartida(NumNavios_J2, NumNavios_B2, Continue2),
        
        (Continue2 -> 
            write('\nVez do bot...\n'),
		    sleep(0.9),
		    disparaNoTabuleiroJogador(Tab_J_Final, Tab_B_Ve_J_Final, Tab_BF, Tab_J_Ve_BF, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2),
            iniciaJogoComMaquina(Tab_JF, Tab_J_Ve_BF2, Tab_BF2, Tab_B_Ve_JF, TamTab, Dados); 
            write('Você quer jogar novamente? [1 para sim, outro número para sair]'),
            ler_opcao(Op),
            (Op =:= 1 -> doWhile(true, Dados, TamTab); menu(Dados)));
        
        write('Você quer jogar novamente? [1 para sim, outro número para sair]'),
        ler_opcao(Op),
        (Op =:= 1 -> doWhile(true, Dados, TamTab); menu(Dados))
        ).


% regra para contagem de navios no tabuleiro
contaNavios([H|[]], NumNaviosFinal):- contaNaviosLinha(H, NumNaviosFinal).
contaNavios([H|T], NumNaviosFinal):-
    contaNaviosLinha(H, NumNavios),
	contaNavios(T, NumNavios2),
	NumNaviosFinal is NumNavios + NumNavios2.

% regra para contagem de navios em uma determinada linha do tabuleiro
contaNaviosLinha([~|[]], 0).
contaNaviosLinha([x|[]], 0).
contaNaviosLinha([o|[]], 0).
contaNaviosLinha(['B'|[]], 0).
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
contaNaviosLinha(['B'|T], NumNaviosFinal):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosFinal is 0 + NumNavios.
contaNaviosLinha([#|T], NumNaviosFinal):-
	contaNaviosLinha(T, NumNavios),
	NumNaviosFinal is 1 + NumNavios.


% regra que verifica se o bot venceu
verificaFinalizacaoPartida(0, K, false):-
	K \= 0,
    writeln('Que pena você perdeu! Seus navios foram para o fundo do mar!').

% regra que verifica se o jogador venceu
verificaFinalizacaoPartida(_, 0, false):-
    writeln('Você é um verdadeiro almirante! Parabéns pela vitória na batalha naval.').

verificaFinalizacaoPartida(A, B, true):- A =\= 0, B =\= 0.


% regra de "ataque" no tabuleiro (bot)
disparaNoTabuleiroBot(Tab_B, Tab_J_Ve_B, Tab_J, Tab_B_Ve_J, TamTab, Tab_Bot_Final, Tab_Jogador_Ve_Bot_Final, Tab_J_Final, Tab_B_Ve_J_Final):-
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
		posicionaSimboloNoNavio(Tab_B, Tab_J_Ve_B, Tab_J, Tab_B_Ve_J, X, Y, Simbolo, TamTab, Tab_Bot_Final, Tab_Jogador_Ve_Bot_Final, Tab_J_Final, Tab_B_Ve_J_Final)
	);
	write('Posicao invalida, digite uma nova posicao valida.\n'),
	disparaNoTabuleiroBot(Tab_B, Tab_J_Ve_B, Tab_J, Tab_B_Ve_J, TamTab, Tab_Bot_Final, Tab_Jogador_Ve_Bot_Final, Tab_J_Final, Tab_B_Ve_J_Final).


% regra que verifica se uma determinada posição já foi usada pelo usuario
verificaSeJaFoiDisparadoNoBot(Tab, X, Y, R):-
    nth1(X, Tab, Linha),
	nth1(Y, Linha, Elemento),
	foiDisparadoNoBot(Elemento, R).


foiDisparadoNoBot(x, false).
foiDisparadoNoBot(o, false).
foiDisparadoNoBot(#, true).
foiDisparadoNoBot(~, true).
foiDisparadoNoBot('B', true).
foiDisparadoNoBot(E, R):- E \= x, E \= o, R = true.


selecionaSimboloNavio(Tab, X, Y, SimboloFinal):-
	nth1(X, Tab, Linha),
	nth1(Y, Linha, Simbolo),
	simboloRetornado(Simbolo, SimboloFinal).

simboloRetornado(#, x).
simboloRetornado(~, o).
simboloRetornado('B', 'B').

% regra que adiciona o simbolo no tabuleiro que indica que achou ou não um navio durante o jogo
posicionaSimboloNoNavio(Tab_B, Tab_J_Ve_B, Tab_J, Tab_B_Ve_J, X, Y, x, TamTab, Tab_Bot_Final, Tab_J_Ve_Bot_Final, Tab_J, Tab_B_Ve_J):-
	write('Voce acertou um navio!\n'),
	posicionaSimbolo(Tab_B, X, Y, x, TamTab, Tab_Bot_Final),
	posicionaSimbolo(Tab_J_Ve_B, X, Y, x, TamTab, Tab_J_Ve_Bot_Final).

posicionaSimboloNoNavio(Tab_B, Tab_J_Ve_B, Tab_J, Tab_B_Ve_J, X, Y, o, TamTab, Tab_B, Tab_J_Ve_Bot_Final, Tab_J, Tab_B_Ve_J):-
	write('Voce acertou na agua!\n'),
	posicionaSimbolo(Tab_J_Ve_B, X, Y, o, TamTab, Tab_J_Ve_Bot_Final).

posicionaSimboloNoNavio(Tab_B, Tab_J_Ve_B, Tab_J, Tab_B_Ve_J, X, Y, 'B', TamTab, Tab_B2, Tab_J_Ve_Bot_Final2, Tab_J_Final, Tab_B_Ve_J_Final):-
	write('Voce acertou uma bomba!\n'),
	posicionaSimbolo(Tab_J_Ve_B, X, Y, 'B', TamTab, Tab_J_Ve_Bot_Final),
    disparaNoTabuleiroJogadorAcertandoNavio(Tab_J, Tab_B_Ve_J, Tab_B, Tab_J_Ve_Bot_Final, TamTab, Tab_J_Final, Tab_B_Ve_J_Final, Tab_B2, Tab_J_Ve_Bot_Final2).

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

% regra que cria/modifica uma linha do tabuleiro com o simbolo adicionado  
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


% regra de "ataque" no tabuleiro (jogadores)
disparaNoTabuleiroJogador(Tab_J, Tab_B_Ve_J, Tab_BF, Tab_J_Ve_BF, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2):-
    Tamanho is TamTab + 1,
	random(1, Tamanho, X),
	random(1, Tamanho, Y),

	verificaJaDisparadoAoJogador(Tab_B_Ve_J, X, Y, Tamanho, Resposta),

	Resposta -> (
		selecionaSimboloNavio(Tab_J, X, Y, Simbolo),
		posicionaSimboloNoNavio(Tab_J, Tab_B_Ve_J, Tab_BF, Tab_J_Ve_BF, X, Y, Simbolo, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2)
	);
	disparaNoTabuleiroJogador(Tab_J, Tab_B_Ve_J, Tab_BF, Tab_J_Ve_BF, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2).


% regra de "ataque" no tabuleiro (jogadores)
disparaNoTabuleiroJogadorAcertandoNavio(Tab_J, Tab_B_Ve_J, Tab_BF, Tab_J_Ve_BF, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2):-
    Tamanho is TamTab + 1,
	random(1, Tamanho, X),
	random(1, Tamanho, Y),

	verificaJaDisparadoAoJogador(Tab_B_Ve_J, X, Y, Tamanho, Resposta),

	Resposta -> (
		selecionaSimboloNavio(Tab_J, X, Y, Simbolo),
        verificaSimboloNavio(Simbolo, EhSimbolo),
        (EhSimbolo -> posicionaSimboloNoNavio(Tab_J, Tab_B_Ve_J, Tab_BF, Tab_J_Ve_BF, X, Y, Simbolo, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2);
    disparaNoTabuleiroJogadorAcertandoNavio(Tab_J, Tab_B_Ve_J, Tab_BF, Tab_J_Ve_BF, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2))
	);
	disparaNoTabuleiroJogadorAcertandoNavio(Tab_J, Tab_B_Ve_J, Tab_BF, Tab_J_Ve_BF, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2).

verificaSimboloNavio(x, true):- !.
verificaSimboloNavio(_, false).

verificaJaDisparadoAoJogador(Tab, X, Y, Tamanho, R):-
	(X > 0, X < Tamanho, Y > 0, Y < Tamanho) -> (
		nth1(X, Tab, Linha),
		nth1(Y, Linha, Elemento),
		foiDisparadoAoJogador(Elemento, R)
	);
	R = false.


foiDisparadoAoJogador(x, false).
foiDisparadoAoJogador(o, false).
foiDisparadoAoJogador(#, true).
foiDisparadoAoJogador(~, true).


% regra que ira posicionar os navios na horizontal
posicionaNavios(Tab, X, Y, TamNavio, TamTab, 0, R):- posicionaNaviosHorizontal(Tab, X, Y, TamNavio, TamTab, R).

% regra que ira posicionar os navios na vertical
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
	montaListaComNavio(Head, 0, TamTab, LimiteMin, LimiteMax, [], true, Resultado).

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
    posicionaNaviosTabJogador(Tab, TamNavios, TamTab, TabComNaviosResul),
    jogaBombas(TabComNaviosResul, TamTab // 5, TamTab, TabResul).


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


chamaJogador(Dados, NomeJogador, JogadorNum, JogadorEscolhido) :-
    (
        JogadorNum = "0", Mensagem = "Você deseja jogar com um jogador já cadastrado? (Digite S para sim e N para não)";
        JogadorNum = "1", Mensagem = "Você deseja jogar com o primeiro jogador já cadastrado? (Digite S para sim e N para não)";
        JogadorNum = "2", Mensagem = "Você deseja jogar com o segundo jogador já cadastrado? (Digite S para sim e N para não)";
        Mensagem = "Opção inválida"
    ),
    writeln(Mensagem),
    writeln("\n→ Opção:"),
    read(Op),
    (
        Op = 'S' ->
        (
            JogadorNum = "1" ->
            (
                writeln("\nDigite o nome do primeiro jogador:"),
                read(Nome1),
                (
                    existeJogador('dados.txt', Nome1) ->
                    JogadorEscolhido = Nome1
                    ;
                    writeln("\nEsse jogador não existe."),
                    sleep(1),
                    chamaJogador(Dados, NomeJogador, JogadorNum, JogadorEscolhido)
                )
            )
            ;
            JogadorNum = "2" ->
            (
                writeln("\nDigite o nome do segundo jogador:"),
                read(Nome2),
                (
                    existeJogador('dados.txt', Nome2) ->
                    JogadorEscolhido = Nome2
                    ;
                    writeln("\nEsse jogador não existe."),
                    sleep(1),
                    chamaJogador(Dados, NomeJogador, JogadorNum, JogadorEscolhido)
                )
            )
        )
        ;
        Op = 'N' ->
        read(Nome),
        JogadorEscolhido = Nome
        ;
        writeln("\nOpção Inválida"),
        sleep(1),
        chamaJogador(Dados, NomeJogador, JogadorNum, JogadorEscolhido)
    ).


existeJogador(Dados, Nome) :-
    member(jogador(Nome, _), Dados).
    

iniciaJogoComJogadores(Tab_Jog1, Tab_J_Ve_J2, Tab_Jog2, Tab_J_Ve_J1, TamTab, Dados, NovosDados) :-
    contaNavios(Tab_Jog1, NumNavios_J1),
    contaNavios(Tab_Jog2, NumNavios_J2),
    
    verificaFinalizacaoPartidaComJogadores(NumNavios_J1, NumNavios_J2, Continue),
    
    (Continue -> 
        shell(clear),
        writeln('Esse é o tabuleiro que o Jogador 1 vai jogar: \n'), % Mudar Frase
        preparaTabParaPrint(Tab_J_Ve_J2, 0, TamTab, Tab_J_R),
        write(Tab_J_R),
        writeln('\nEsse é o tabuleiro que o Jogador 2 vai jogar: \n'),
        preparaTabParaPrint(Tab_J_Ve_J1, 0, TamTab, Tab_B_R),
        write(Tab_B_R),
        write('Numero de navios restantes do jogador 1: '), write(NumNavios_J1), write('\n'),
        write('Numero de navios restantes do jogador 2: '), write(NumNavios_J2), write('\n\n'),
    
        write('\nVez do jogador 1...\n'),
        disparaNoTabuleiroBot(Tab_Jog2, Tab_J_Ve_J2, Tab_Jog1, Tab_J_Ve_J1, TamTab, Tab_BF, Tab_J_Ve_BF, Tab_J1_Final, Tab_B_Ve_J_Final),

        contaNavios(Tab_J1_Final, NumNavios_J1_2),
        contaNavios(Tab_BF, NumNavios_J2_2),
        verificaFinalizacaoPartidaComJogadores(NumNavios_J1_2, NumNavios_J2_2, Continue2),
        (Continue2 -> 
            write('\nVez do jogador 2...\n'),
            sleep(0.9),
            disparaNoTabuleiroBot(Tab_J1_Final, Tab_B_Ve_J_Final, Tab_BF, Tab_J_Ve_BF, TamTab, Tab_JF, Tab_B_Ve_JF, Tab_BF2, Tab_J_Ve_BF2),
            iniciaJogoComJogadores(Tab_JF, Tab_J_Ve_BF2, Tab_BF2, Tab_B_Ve_JF, TamTab, Dados, NovosDados); 
            write('Você quer jogar novamente? [1 para sim, outro número para sair]'),
            ler_opcao(Op),
            (Op =:= 1 -> doWhile(true, Dados, TamTab); menu(Dados))
        );
        write('Você quer jogar novamente? [1 para sim, outro número para sair]'),
        ler_opcao(Op),
        (Op =:= 1 -> doWhile(true, Dados, TamTab); menu(Dados))  
    ).    


verificaFinalizacaoPartidaComJogadores(0, K, false):-
	K \= 0,
    writeln('Parabéns o jogador 2 ganhou!').

verificaFinalizacaoPartidaComJogadores(_, 0, false):-
    writeln('Parabéns o jogador 1 ganhou!').

verificaFinalizacaoPartidaComJogadores(A, B, true):- A =\= 0, B =\= 0.

% regra que exibe ranking
lerArquivo(NomeArquivo) :-
    open(NomeArquivo, read, Stream),
    lerLinhas(Stream, Linhas),
    close(Stream),
    delete(Linhas, end_of_file, LinhasFiltradas),
    montarRanking(LinhasFiltradas).

lerLinhas(Stream, Linhas) :-
    lerLinhasAux(Stream, [], Linhas).

lerLinhasAux(Stream, LinhasTemp, Linhas) :-
    at_end_of_stream(Stream),
    reverse(LinhasTemp, Linhas),
    !.

lerLinhasAux(Stream, LinhasTemp, Linhas) :-
    \+ at_end_of_stream(Stream),
    lerLinha(Stream, Linha),
    lerLinhasAux(Stream, [Linha|LinhasTemp], Linhas).

lerLinha(Stream, Linha) :-
    read(Stream, Linha).

montarRanking(Linhas) :-
    delete(Linhas, end_of_file, LinhasFiltradas),
    sort(2, @>=, LinhasFiltradas, Ranking),
    exibirRanking(Ranking).

exibirRanking([]).
exibirRanking([jogador(Nome, Pontos)|Resto]) :-
    format('O jogador ~w possui ~w pontos~n', [Nome, Pontos]),
    exibirRanking(Resto).


% regra para lê uma opção do usuario
ler_opcao(Op):-
    write('→ Opção:'),nl,
    read(Op).

% Negação do Bool
negateBool(false, true).
negateBool(true, false).

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

% regra que verifica se tem uma parte do navio em uma determinada posição do tabuleiro
temNavio([], false).
temNavio(['#'|_], true).
temNavio(['B'|_], true).
temNavio([~|T], R):- temNavio(T, R).
temNavio([x|T], R):- temNavio(T, R).
temNavio([o|T], R):- temNavio(T, R).

% Implementando a função transpose de haskell em prolog
transpose([[]|_], []).
transpose(Tab, [Linha|Linha2]) :- transpose_coluna(Tab, Linha, TabResul),
                                 transpose(TabResul, Linha2).
transpose_coluna([], [], []).
transpose_coluna([[H|T]|Linha2], [H|Hs], [T|Ts]) :- transpose_coluna(Linha2, Hs, Ts).

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
    (TamTab < 10 -> 
        string_concat(" ", TamTab, RAux),
        string_concat(RAux, " ", R1);
        string_concat(TamTab, " ", R1)),
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

verificaPosicaoValida(Tab, X, Y, TamanhoTab, Result) :-
  X < TamanhoTab,
  Y < TamanhoTab,
  X >= 0,
  Y >= 0,
  verificaTemNavio(Tab, X, Y, 1, PosOcupada),
  (PosOcupada = true -> Result = false ; Result = true), !.
verificaPosicaoValida(_, _, _, _, false).

verificaTemNavio(Tab, X, Y, 1, PosOcupada):-
    X2 is X - 1,
    Y2 is Y - 1,
    nth0(X2, Tab, Linha),
    drop(Y2, Linha, LinhaDrop),
    take(1, LinhaDrop, LinhaDropTake),
    temNavio(LinhaDropTake, PosOcupada).


jogaBombas(Tab, 0, _, Tab).
jogaBombas(Tab, QtdBombas, TamanhoTab, Result) :-
    Tamanho is TamanhoTab + 1,
    random(0, Tamanho, X),
    random(0, Tamanho, Y),

    verificaPosicaoValida(Tab, X, Y, Tamanho, PosValida),
    (PosValida 
        ->  NewQtdBombas is (QtdBombas - 1),
            adicionaBomba(Tab, X, Y, TamanhoTab, NewTab),
            jogaBombas(NewTab, NewQtdBombas, TamanhoTab, Result)
        ; jogaBombas(Tab, QtdBombas, TamanhoTab, Result)).


adicionaBomba(Tab, X, Y, TamanhoTab, Result) :-
    adicionaBombaMatriz(Tab, X, Y, 1, TamanhoTab, Result).

adicionaBombaMatriz(Tab, X, Y, TamNavio, TamTab, Resultado):-
    X2 is X - 1,
    Y2 is Y - 1,
    adicionaBombaLista(Tab, TamNavio, TamTab, X2, Y2, Resultado).


adicionaBombaLista([Head|[]], TamNavio, TamTab, 0, Posicao_Y, Resultado):- 
	LimiteMin = Posicao_Y,
	LimiteMax is Posicao_Y + TamNavio - 1, 
	montaListaComBomba(Head, 0, TamTab, LimiteMin, LimiteMax, [], true, Resultado).

adicionaBombaLista([H|T], TamNavio, TamTab, 0, Posicao_Y, Resultado):-
	LimiteMin = Posicao_Y,
	LimiteMax is Posicao_Y + TamNavio - 1, 
	montaListaComBomba(H, 0, TamTab, LimiteMin, LimiteMax, [], R),                   
	adicionaBombaLista(T, TamNavio, TamTab, -1, Posicao_Y, R2),
	append([R], R2, Resultado).

adicionaBombaLista([H|[]], _, _, _, _, [H]). 

adicionaBombaLista([H|T], TamNavio, TamTab, Posicao_X, Posicao_Y, Resultado):-
  Pos_X_Aux is Posicao_X - 1,
  adicionaBombaLista(T, TamNavio, TamTab, Pos_X_Aux, Posicao_Y, Resul),
	append([H], Resul, Resultado).

montaListaComBomba(_, TamTab, TamTab, _, _, NovaLista, true, [NovaLista]).
montaListaComBomba(Lista, I, TamTab, MinI, MaxI, ListaResul, true, R):-
	I >= 0,
	I < TamTab,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, Lista, Elemento),
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComBomba(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, true, R));
		(Elemento = 'B',
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComBomba(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, true, R)).

montaListaComBomba(_, TamTab, TamTab, _, _, NovaLista, NovaLista).
montaListaComBomba(Lista, I, TamTab, MinI, MaxI, ListaResul, R):-
	I >= 0,
	I < TamTab,
	((I < MinI); (I > MaxI)) ->
		(nth0(I, Lista, Elemento),
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComBomba(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, R));
		(Elemento = 'B',
		NovoI is I + 1,
		append(ListaResul, [Elemento], NovoLSaida),
		montaListaComBomba(Lista, NovoI, TamTab, MinI, MaxI, NovoLSaida, R)).