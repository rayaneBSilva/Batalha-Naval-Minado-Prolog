:- use_module(library(system)).
:- use_module(library(random)).

% definição dos tipos de dados
type Jogadores = [Jogador].
type Nome = string.
type Pontuacao = int.
type Vez = int.
type Tabuleiro = [[char]].
data Jogador = Jogador Nome Pontuacao
                    deriving (Show, Read).


% função que inicia o programa
main :-
    exibirConteudoArquivo('introducao.txt').
    inicio_apresentacao([]),nl,
    halt.

% ---------- INTRODUÇÃO E HISTÓRIA ----------

exibirConteudoArquivo(NomeArquivo) :-
    open(NomeArquivo, read, Stream),
    repeat,
    read_line_to_codes(Stream, Conteudo),
    (   Conteudo \= end_of_file
    ->  format('~s~n', [Conteudo]),
        fail
    ;   true
    ),
    close(Stream).


% Função que apresenta uma introdução ao jogo e chama o menu
inicio_apresentacao(Dados) :-
    imprimiIntroducao,
    write('\n\nPressione <Enter> para continuar'),
    read_line(_),
    menu(Dados).

% função que irá imprimir a introdução do jogo
imprimiIntroducao :-
    system('clear'), % limpa a tela
    open('introducao.txt', read, Handle),
    imprimiTextoLentamente(Handle),
    close(Handle).

% função que irá imprimir a história do jogo
imprimiHistoria(Dados) :-
    system('clear'), % limpa a tela
    open('historia.txt', read, Handle),
    imprimiTextoLentamente(Handle),
    close(Handle),
    menu(Dados).

% função para imprimir lentamente a introdução do jogo
imprimiTextoLentamente(Handle) :-
    read_line(Handle, Texto),
    imprimiTextoLentamenteAux(Texto, Handle).

imprimiTextoLentamenteAux([], _) :- !.
imprimiTextoLentamenteAux([Char|Texto], Handle) :-
    put_code(Char),
    sleep(0.05), % aguarda 50ms
    imprimiTextoLentamenteAux(Texto, Handle).


% ---------- MENU ----------

% função que exibe o Menu
menu(Dados) :-
    system('clear'), % limpa a tela
    writeln('-------------------- Batalha Naval --------------------'),
    writeln('\n ● Digite 1 para cadastrar jogador'),
    writeln('\n ● Digite 2 para jogar'),
    writeln('\n ● Digite 3 para visualizar o ranking'),
    writeln('\n ● Digite 4 para ver o modo história'),
    writeln('\n ● Digite 0 para sair '),
    write('\n→ Opção: '),
    read_line(Op),
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