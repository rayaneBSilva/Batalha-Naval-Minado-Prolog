:- dynamic(jogador/1).
:- dynamic(tabuleiro/1).

% Função que exibe o conteúdo de um arquivo
exibirConteudoArquivo(NomeArquivo) :-
    open(NomeArquivo, read, Stream),
    exibirConteudoArquivoAux(Stream),
    close(Stream).

exibirConteudoArquivoAux(Stream) :-
    read_line_to_string(Stream, Linha),
    (   Linha \= end_of_file
    ->  writeln(Linha),
        exibirConteudoArquivoAux(Stream)
    ;   true
    ).

% Função que exibe o menu principal
exibirMenu :-
    write('-------------------- Batalha Naval --------------------'), nl,
    write('\n ● Digite 1 para cadastrar jogador'), nl,
    write('\n ● Digite 2 para jogar'), nl,
    write('\n ● Digite 3 para visualizar o ranking'), nl,
    write('\n ● Digite 4 para ver o modo história'), nl,
    write('\n ● Digite 0 para sair \n').

% Função para ler a opção escolhida pelo usuário
lerOpcao(Opcao) :-
    write("→ Opção: "),
    read(Opcao).


% Função para manipular a opção escolhida pelo usuário
executarOpcao(0) :-
    write('\nA água esquece o nome dos afogados...'), nl,
    encerrar.
executarOpcao(1) :-
    cadastrarJogador, menu.
executarOpcao(2) :-
    prepararJogo.
executarOpcao(4) :-
    exibirConteudoArquivo('historia.txt'),
    write("Pressione qualquer tecla para voltar ao menu"),
    read(_),
    menu.
executarOpcao(_) :-
    write("Opção inválida!").

% Função para cadastrar um jogador
cadastrarJogador :-
    write("Digite o nome do jogador: "),
    read(Nome),
    assert(jogador(Nome)),
    write("Jogador cadastrado com sucesso!").
% Função para cadastrar um jogador
cadastrarJogador :-
    write("Digite o nome do jogador: "),
    read(Nome),
    assert(jogador(Nome)),
    write("Jogador cadastrado com sucesso!").

% Função para preparar o jogo
prepararJogo :-
    write('-------------------- Batalha Naval --------------------'), nl,
    write('\n ● Digite 1 para jogar com a máquina'), nl,
    write('\n ● Digite 2 para jogar com dois jogadores'), nl,
    write('\n ● Digite 3 para redimensionar o tabuleiro'), nl,
    write('\n ● Digite 0 para voltar ao menu\n'),
    lerOpcao(Opcao),
    (   executarOpcao(Opcao),
        encerrar
    ;   menu
    ).

% Função para encerrar o programa
encerrar :- halt.

% Função para jogar com a máquina
jogarComMaquina :-
    write("Jogando com a máquina...").

% Função para jogar com dois jogadores
jogarComDoisJogadores :-
    write("Jogando com dois jogadores...").


% Função para redimensionar o tabuleiro
redimensionarTabuleiro(Tamanho) :-
    write("Qual o novo tamanho do tabuleiro? (Digite somente um valor, ex: 10): "),
    read(NovoTamanho),
    (   integer(NovoTamanho),
        NovoTamanho > 0
    ->  (
            write("Redimensionando tabuleiro..."),
            nl,
            sleep(2),
            atualizarTabuleiro(NovoTamanho),
            write("Tabuleiro redimensionado com sucesso!"),
            nl,
            sleep(2),
            nl,
            menu
        )
    ;   write("Tamanho inválido! Por favor, digite um número inteiro positivo."), nl,
        redimensionarTabuleiro(Tamanho)
    ).




% Função para atualizar o tamanho do tabuleiro
atualizarTabuleiro(Tamanho) :-
    (   retract(tabuleiro(_))
    ->  true
    ;   true
    ),
    assert(tabuleiro(Tamanho)).

% Função principal
main :-
    exibirConteudoArquivo('introducao.txt'),
    sleep(2),
    menu.

% Função que exibe o menu e lê a opção do usuário
menu :-
    exibirMenu,
    lerOpcao(Opcao),
    executarOpcao(Opcao),
    !. % Corte para evitar execução de outras cláusulas em menu/0

