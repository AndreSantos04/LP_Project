% 107041 Andre Antunes Santos
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.



/* Funcao Auxiliar: Transforma um periodo para o semestre em que e lecionado ou
            um semestre num dos periodos possiveis dentro do mesmo*/

semestre_para_Periodos(Semestre,Periodo):-
        Semestre == p1_2,(Periodo = p1;Periodo = p2);
        Semestre == p3_4,(Periodo = p3;Periodo = p4);
        (Periodo == p1;Periodo == p2), Semestre = p1_2;
        (Periodo == p3;Periodo == p4), Semestre = p3_4.



%%%%%%%%%%%%%%%%%%%%%%%%%%%    Qualidade dos Dados    %%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
eventosSemSalas(EventosSemSala)
EventosSemSala - Lista dos IDs de todos os eventos sem sala atribuida
*/
eventosSemSalas(EventosSemSala):-
        findall(ID,evento(ID,_,_,_,'semSala'),EventosSemSala).


/*
eventosSemSalasDiaSemana(DiaDaSemana,EventosSemSala)
EventosSemSala - Lista dos IDs de todos os eventos sem sala atribuida 
                num determinado DiaDaSemana

Verifica para cada evento que ocorre no DiaDaSemana se pertence a
lista de IDs dos eventos sem sala
*/
eventosSemSalasDiaSemana(DiaDaSemana,EventosSemSala):-
        eventosSemSalas(Eventos),
        findall(ID,(horario(ID,DiaDaSemana,_,_,_,_),
        member(ID,Eventos)),EventosSemSala).


/*
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala)
EventosSemSala - Lista dos IDs de todos os eventos sem sala atribuida 
                nos periodos de ListaPeriodos

Um periodo pode ser um periodo simples(p1) ou um semestre(p1_2), no caso de
ser semestre o mesmo pertence aos dois periodos nele contidos(p1 e p2)
*/
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala):- 
        eventosSemSalas(Eventos),
        findall(ID,(horario(ID,_,_,_,_,Periodo), 
        (member(Periodo,ListaPeriodos);
        %Se o ID equivaler a um semestre, verifica se algum dos periodos pertence
        (semestre_para_Periodos(Periodo,Periodo_Separado),
        member(Periodo_Separado,ListaPeriodos))),
        member(ID,Eventos)), EventosSemSala_Com_Repeticoes),
        sort(EventosSemSala_Com_Repeticoes, EventosSemSala).
                                                

%%%%%%%%%%%%%%%%%%%%%%%%%%%     Pesquisas simples     %%%%%%%%%%%%%%%%%%%%%%%%%%% 

/*
organizaEventos(ListaEventos,Periodo,EventosNoPeriodo)
EventosNoPeriodo - Lista de IDs de ListaEventos cujo evento ocorre no Periodo
*/
%Caso terminal - ja nao existem mais IDs
organizaEventos([],_,[]).

%Analisa se o evento com o ID assinalado ocorre no Periodo e adiciona a lista
organizaEventos([ID|Restantes],Periodo,EventosNoPeriodo):-
        (horario(ID,_,_,_,_,Periodo);
        %O periodo do ID pode equivaler a um Semestre que contem o Periodo
        (semestre_para_Periodos(Semestre,Periodo), 
        horario(ID,_,_,_,_,Semestre))),
        organizaEventos(Restantes,Periodo,MaisEventos),
        merge([ID], MaisEventos, EventosNoPeriodo).

%Caso nao seja ocorra para o ID seguinte
organizaEventos([_|Restantes],Periodo,EventosNoPeriodo):-
        organizaEventos(Restantes,Periodo,EventosNoPeriodo).


/*
eventosMenoresQue(Duracao,ListaEventosMenoresQue)
ListaEventosMenoresQue - Lista de IDs cuja duracao do evento e menor ou igual
a Duracao
*/
eventosMenoresQue(Duracao,ListaEventosMenoresQue):-
        findall(ID,(horario(ID,_,_,_,DuracaoEvento,_),
        DuracaoEvento=<Duracao),ListaDesorganizada),
        sort(ListaDesorganizada,ListaEventosMenoresQue).


/*
eventosMenoresQueBool(ID,Duracao)
Devolve True se a duracao do evento do ID e menor ou igual a Duracao

Verifica se o ID pertence a ListaEventosMenoresQue 
*/

eventosMenoresQueBool(ID,Duracao):-
        eventosMenoresQue(Duracao,ListaEventosMenoresQue),
        member(ID,ListaEventosMenoresQue).

/*
procuraDisciplinas(Curso,ListaDisciplinas)
ListaDisciplinas - Lista de todas as disciplinas de um Curso
*/

procuraDisciplinas(Curso,ListaDisciplinas):-
        findall(Disciplina,(turno(ID,Curso,_,_), 
        evento(ID,Disciplina,_,_,_)),ListaDesorganizada),
        sort(ListaDesorganizada,ListaDisciplinas).


/*
organizaDisicplinas(ListaDisciplinas,Curso,Semestres)
Semestres - Lista de listas das disciplinas de ListaDisciplinas de um curso
separadas pelos semestres em que sao lecionadas: 
a primeira lista equivale ao Semestre 1 e a segunda ao Semestre 2

Cria um predicado auxiliar que recebe a ListaDisciplinas e cria duas listas
separadas, uma para cada semestre, com as respetivas disciplinas 
*/
%Caso terminal - ja nao existem mais disciplinas
organizaDisciplinas1([],_,_,_,_,_). 

%Verifica a que semestre pertence a Disciplina e junta-a a respetiva lista
organizaDisciplinas1([Disciplina|DiscRestantes],Curso,Sem1,Sem2,DiscSem1,DiscSem2):-                                                                        
    evento(ID,Disciplina,_,_,_),turno(ID,Curso,_,_),horario(ID,_,_,_,_,Periodo),                                                                        
    (((Periodo == 'p1_2';(semestre_para_Periodos(S,Periodo),S == 'p1_2')),
    merge([Disciplina],Sem1,LSem1Inacabado),!);
    ((Periodo == 'p3_4';(semestre_para_Periodos(S,Periodo),S == 'p3_4')),
    merge([Disciplina],Sem2,LSem2Inacabado))),
    organizaDisciplinas1(DiscRestantes, Curso, LSem1Inacabado, LSem2Inacabado,_,_),
    organizaDisciplinas1(DiscRestantes, Curso, LSem1Inacabado, LSem2Inacabado,L1,L2),
    merge(LSem1Inacabado,L1,DiscSem1),merge(LSem2Inacabado,L2,DiscSem2).                                                                                                                                          

%Junta as duas listas formadas numa lista de listas
organizaDisciplinas(ListaDisciplinas,Curso,Semestres):-
    organizaDisciplinas1(ListaDisciplinas,Curso,[],[],DiscSem1,DiscSem2),                                                    
    sort(DiscSem1,Semestre1),sort(DiscSem2,Semestre2),
    merge([Semestre1],[Semestre2],Semestres),!.
   

/*
horasCurso(Periodo,Curso,Ano,TotalHoras)
TotalHoras - Numero total de horas de um Curso num determinado Ano e Periodo

Cria uma lista com os IDs dos eventos do Curso no Ano e Periodo respetivos e
utiliza o predicado auxiliar horasPorEvento para somar o numero de horas 
de cada evento
*/

horasCurso(Periodo,Curso,Ano,TotalHoras):-
        findall(ID,(turno(ID,Curso,Ano,_), 
        (horario(ID,_,_,_,_,Periodo);
        (semestre_para_Periodos(Semestre,Periodo),
        horario(ID,_,_,_,_,Semestre)))),ListaIDsDesorganizada),
        sort(ListaIDsDesorganizada,ListaIDs),
        horasPorEvento(ListaIDs,TotalHoras).

horasPorEvento([],0).
horasPorEvento([ID|IDsRestantes],TotalHoras):-
        horario(ID,_,_,_,Duracao,_),
        horasPorEvento(IDsRestantes,Horas),
        TotalHoras is Duracao + Horas.

/*
evolucaoHorasCurso(Cursos,Evolucao)
Evolucao - Lista de tuplos (Ano,Periodo,NumHoras) com a evolucao do numero de
horas de um Curso em cada periodo ao longo dos anos
*/
evolucaoHorasCurso(Curso,Evolucao):-
        findall((Ano,Periodo,NumHoras),(between(1,3,Ano),
        ((between(1, 4, P),numParaPeriodo(P,Periodo),
        horasCurso(Periodo,Curso,Ano,NumHoras)))),Evolucao).

%Predicado auxiliar - transforma um numero (1 a 4) no periodo correspondente
numParaPeriodo(Num,Periodo):-
        Num == 1,Periodo= 'p1';
        Num == 2,Periodo= 'p2';
        Num == 3,Periodo= 'p3';
        Num == 4,Periodo= 'p4'.

%%%%%%%%%%%%%%%%%%%%       Ocupacao Critica Das Salas       %%%%%%%%%%%%%%%%%%%%

/*
ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas)
Horas - Calcula o tempo que um evento se sobrepoe a um intervalo de tempo dado 
*/

ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas):-
        Horas is min(HoraFimEvento, HoraFimDada)-max(HoraInicioEvento, HoraInicioDada),
        HoraInicioDada<HoraFimEvento,HoraFimDada>HoraInicioEvento.

/*
numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras)
SomaHoras - numero de horas ocupadas das salas de um TipoSala
entre HoraInicio e HoraFim de um Periodo e DiaSemana

Cria uma lista com o numero de horas ocupadas por cada sala dentro do intervalo
de tempo e soma os elementos dessa lista atraves de soma_elementos
*/

numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras):-
        salas(TipoSala,ListaSalas),
        findall(Horas,((evento(ID,_,_,_,Sala),member(Sala,ListaSalas)),
        (horario(ID,DiaSemana,HoraInicioEvento,HoraFimEvento,_,Periodo);
        semestre_para_Periodos(Semestre,Periodo),
        horario(ID,DiaSemana,HoraInicioEvento,HoraFimEvento,_,Semestre)),
        ocupaSlot(HoraInicio,HoraFim,HoraInicioEvento,HoraFimEvento,Horas)),
        ListaHoras),soma_elementos(ListaHoras, SomaHoras).

%Predicado Auxiliar - soma todos os elementos de uma lista
soma_elementos([],0).
soma_elementos([X|Y],Soma):-soma_elementos(Y,Soma1),Soma is Soma1+X.  

/*
ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max)
Max - Numero maximo de horas ocupadas que um tipo de sala pode ter  
num determinado intervalo de tempo
*/
ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max):-
        salas(TipoSala,ListaSalas), 
        length(ListaSalas, NumSalas),
        Max is (HoraFim-HoraInicio)*NumSalas.

/*
percentagem(SomaHoras,Max,Percentagem)	
Percentagem - Percentagem de horas ocupadas face ao numero maximo de horas
*/
percentagem(SomaHoras,Max,Percentagem):- 
        Percentagem is (SomaHoras/Max*100).
  
/*
ocupacaoCritica(HoraInicio,HoraFim,Threshold,Resultados)
Resultados - tuplo do tipo casosCriticos(DiaSemana,TipoSala,Percentagem),
Percentagem - Percentagem de ocupacao, num dado intervalo, de um TipoSala 
num DiaSemana 

Regista um casoCritico se a Percentagem superior a Threshold
Analisa casos criticos em cada periodo
*/

ocupacaoCritica(HoraInicio,HoraFim,Threshold,Resultados):-
        findall((casosCriticos(DiaSemana,TipoSala,PercentagemArred)),
        (between(2,6,Num),(numParaDiaSemana(Num,DiaSemana), %Cada DiaSemana
        between(1,4,P),numParaPeriodo(P,Periodo), %Cada Periodo
        (ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max),
        numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras),
        not(SomaHoras==0),
        percentagem(SomaHoras,Max,Percentagem),Percentagem>Threshold,
        ceiling(Percentagem, PercentagemArred)))),ResultadosDesorganizados),
        sort(ResultadosDesorganizados,Resultados).

%Funcao auxiliar - Transforma um numero no respetivo DiaSemana
numParaDiaSemana(Num,DiaSemana):-
        Num == 2, DiaSemana= segunda-feira;
        Num == 3, DiaSemana= terca-feira;
        Num == 4, DiaSemana= quarta-feira;
        Num == 5, DiaSemana= quinta-feira;
        Num == 6, DiaSemana= sexta-feira.



%%%%%%%%%%%%%%   And now for something completely different...   %%%%%%%%%%%%%%

/*Mesa:[[X1,X2,X3],[X4,X5],[X6,X7,X8]]

    X1  X2  X3
  X4          X5
    X6  X7  X8

*/

%Predicado que verifica se uma pessoa tem mais de um lugar atribuido
%Se a lista de pessoas sem repeticoes for diferente
%da lista com todas as pessoas, entao existem pessoas com mais de um lugar

pessoaMaisDeUmLugar([L1,Cab,L2],NomesPessoas):-
        merge(L1,Cab,X),merge(X,L2,NomesLugar),sort(NomesLugar,PessoasComLugar),
        length(PessoasComLugar,NumPessoasComLugar),
        length(NomesPessoas,NumPessoas),NumPessoas==NumPessoasComLugar.

%Verifica se uma pessoa ja tem lugar atribuido
%Se nao tiver atribui um lugar

exists([NomePessoa],[L1,Cab,L2]):-
        (member(NomePessoa,[L1,Cab,L2]),!);
        member(NomePessoa,L1);member(NomePessoa,Cab);member(NomePessoa,L2).

%Adiciona o NomePessoa ao lugar correspondente a primeira cabeceira
cab1([NomePessoa],[[_,_,_],[NomePessoa,_],[_,_,_]]). 
%Adiciona o NomePessoa ao lugar correspondente a segunda cabeceira
cab2([NomePessoa],[[_,_,_],[_,NomePessoa],[_,_,_]]). 

%NomePessoa1 fica numa cabeceira e NomePessoa2 fica a sua direita: 
%Se NomePessoa1 estiver na primeira cabeceira, NomePessoa2 fica em X3
%Se NomePessoa1 estiver na segunda cabeceira, NomePessoa2 fica em X5
honra([NomePessoa1,NomePessoa2],[L1,Cab,L2]):-
        (exists([NomePessoa1],[_,Cab,_]),
        nth1(N,Cab,NomePessoa1),
        ((N==1,nth1(1,L2,NomePessoa2));N==2,nth1(3,L1,NomePessoa2)));
        (nth1(3,L1,NomePessoa2),nth1(2,Cab,NomePessoa1));
        (nth1(1,L2,NomePessoa2),nth1(1,Cab,NomePessoa1)).
        
/*
lado([NomePessoa1,NomePessoa2],[L1,_,L2])
Nome Pessoa1 e NomePessoa2 ficam lado a lado em L1 ou em L2
*/

lado_aux([X,Y],[L1,L2]):-
        nextto(X,Y,L1);
        nextto(Y,X,L1);
        nextto(X,Y,L2);
        nextto(Y,X,L2).

lado([NomePessoa1,NomePessoa2],[L1,_,L2]):-
        (exists([NomePessoa1],[L1,_,L2]),
        (lado_aux([NomePessoa1,NomePessoa2],[L1,L2])));
        (exists([NomePessoa2],[L1,_,L2]),
        (lado_aux([NomePessoa2,NomePessoa1],[L1,L2]))).


/*
naoLado([NomePessoa1,NomePessoa2],[L1,Cab,L2])
Nome Pessoa1 e NomePessoa2 nao ficam lado a lado

Se N = 1 entao o elemento X esta na primeira posicao da lista (X1 ou X6)
E o elemento Y pode estar na terceira posicao da lista (X3 ou X8), ou entao
na cabeceira (X4 ou X5) ou em qualquer posicao da outra lista

Se N = 3 entao o elemento X esta na terceira posicao da lista (X3 ou X8)
E o elemento Y pode estar na primeira posicao da lista (X1 ou X6), ou entao
na cabeceira (X4 ou X5) ou em qualquer posicao da outra lista

Se N = 2 entao o elemento X esta na segunda posicao da lista (X2 ou X7)
O elemento Y pode estar na cabeceira (X4 ou X5) 
ou em qualquer posicao da outra lista

Se o elemento X estiver numa cabeceira (X4 ou X5) entao o elemento Y pode estar
em qualquer posicao da lista diferente da posicao de X
*/
naoLado_aux([X,Y],[L1,Cab,L2]):-
        (nth1(N,L1,X), 
        ((N==1,nth1(3,L1,Y));(N==3,nth1(1,L1,Y));
        (member(N,[1,2,3]),(member(Y,Cab));member(Y,L2))));
        (nth1(N,L2,X),
        ((N==1,nth1(3,L2,Y));(N==3,nth1(1,L2,Y));
        (member(N,[1,2,3]),(member(Y,Cab));member(Y,L1))));
        (nth1(_,Cab,X),exists([Y],[L1,Cab,L2])).

naoLado([NomePessoa1,NomePessoa2],[L1,Cab,L2]):-
        (exists([NomePessoa1],[L1,Cab,L2]);exists([NomePessoa2],[L1,Cab,L2])),
        (naoLado_aux([NomePessoa1,NomePessoa2],[L1,Cab,L2]);
        naoLado_aux([NomePessoa2,NomePessoa1],[L1,Cab,L2])).



/*
frente([NomePessoa1,NomePessoa2],[L1,_,L2])
NomePessoa1 e NomePessoa2 ficam frente a frente
*/
frente([NomePessoa1,NomePessoa2],[L1,_,L2]):-
        (nth1(N,L1,NomePessoa1),nth1(N,L2,NomePessoa2));
        (nth1(N,L1,NomePessoa2),nth1(N,L2,NomePessoa1));
        (nth1(N,L2,NomePessoa1),nth1(N,L1,NomePessoa2));
        (nth1(N,L2,NomePessoa2),nth1(N,L1,NomePessoa1)).


/*
naoFrente([NomePessoa1,NomePessoa2],[L1,Cab,L2])
NomePessoa1 e NomePessoa2 nao ficam frente a frente
*/
naoFrente([NomePessoa1,NomePessoa2],[L1,Cab,L2]):-
        (exists(NomePessoa1,[L1,Cab,L2]);exists(NomePessoa2,[L1,Cab,L2])),
        (naoFrente_aux([NomePessoa1,NomePessoa2],[L1,Cab,L2]));
        (naoFrente_aux([NomePessoa2,NomePessoa1],[L1,Cab,L2])).

naoFrente_aux([X,Y],[L1,Cab,L2]):-
        (nth1(N,L1,X),between(1,3,L),L\==N,
        (nth1(L,L2,Y);nth1(L,L1,Y);between(1,2,C),nth1(C,Cab,Y)));
        (nth1(N,L2,X),between(1,3,L),L\==N,
        (nth1(L,L2,Y);nth1(L,L1,Y);between(1,2,C),nth1(C,Cab,Y)));
        (member(X,Cab),exists([Y],[L1,Cab,L2])).

/*
ocupacaoMesa(ListaPessoas,ListaRestricoes,OcupacaoMesa)
ListaPessoas e uma lista de pessoas que irao sentar-se a mesa
ListaRestricoes e uma lista de varias restricoes que as pessoas devem cumprir
OcupacaoMesa e uma lista que representa o lugar que cada pessoa ocupa na mesa

Comeca por ordenar a lista para comecar sempre por colocar as pessoas das 
cabeceiras e dos lugares de honra

Predicado Auxiliar - Atraves do Functor e o predicado call utilizamos cada uma
das restricoes e verifica se o lugar foi atribuido a uma pessoa ja com lugar

Por fim atribui um lugar a quem ainda nao tiver nenhum atribuido
*/
ocupacaoMesa(ListaPessoas,ListaRestricoes,OcupacaoMesa):-
        sort(ListaRestricoes,ListRestOrd),
        ocupacaoMesa_Aux(ListaPessoas,ListRestOrd,OcupacaoMesa,PessoasComLugar),
        findall(Pessoa,(member(Pessoa,ListaPessoas),
        not(member(Pessoa,PessoasComLugar))),PessoasRestantes),
        (PessoasRestantes=[];exists(PessoasRestantes,OcupacaoMesa)),!.
    
ocupacaoMesa_Aux(_,[],_,[]).
ocupacaoMesa_Aux(ListaPessoas,[Restricao|Restantes],OcupacaoMesa,PessoasComLugar):-
        Restricao=..[Rest|Pessoas],call(Rest,Pessoas,OcupacaoMesa),
        pessoaMaisDeUmLugar(OcupacaoMesa,ListaPessoas),
        ocupacaoMesa_Aux(ListaPessoas,Restantes,OcupacaoMesa,PessoasUtilizadas),
        append(Pessoas,PessoasUtilizadas,PessoasComLugar),!.    


    
    