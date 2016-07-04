%% Program pre-processing an OWL/RDF file for the use in ProbLog
%%
%% The four type of probabilities over BEL statements are assigned randomly
%%
%%

:- use_module(library(readutil)).
:- use_module(library(gensym)).

:- reset_gensym.

:- ['BMsemWebTools.pl'].


:- dynamic probFact/3.
:- dynamic modelContent/1.
:- dynamic outputStream/1.
:- dynamic availableThread/1.
:- dynamic addTotalInput/1.

:- prolog_load_context(directory, Dir),  asserta(user:file_search_path(my_home, Dir)).


%% Defining an auxiliary predicate

commandProblog('cmd /c "C:/Users/user/Dysk Google/problog2.1/problog-cli.py"').
%commandProblog('cmd /c "C:/Users/Szymon/Google Drive/problog2.1/problog-cli.py"').
%commandProblog('"C:/Program Files (x86)/Python 3.5/python.exe" "C:/Users/csstssk/Google Drive/problog2.1/problog-cli.py"').


run :- 
    retractall(probFact(_, _, _)),
    reset_gensym,
    readModel,
    openOutput,
    generateNthreads(14),
    writeln('Querying ProbLog...'),
    get_time(Time1),
    findall(_, select_read, _), !, 
    get_time(Time2),
    X is round((Time2 - Time1) / 60), 
    write('Time elapsed: '), writeln(X),
    thread_create(endMonitor, _, [detached(true), alias(controler), at_exit(finalize)]).

endMonitor :- 
    sleep(0.5),
    once((thread_property(_, alias(X)), 
    string_concat('problog_', _, X))) 
        -> endMonitor; 
           true.     

freeWhenDead(Alias) :- 
    once(thread_property(_, alias(Alias)))
        -> freeWhenDead(Alias); 
           asserta(availableThread(Alias)). 

finalize :- 
    outputStream(OutputStream),
    close(OutputStream),
    nl,
    modelContent(DataLine),
    open('problogTotalInput.pl', write, Stream2, [encoding(utf8)]),
    writeln(Stream2, DataLine),
    findall(_, (addTotalInput(InputFile), writeln(Stream2, InputFile)), _),
    close(Stream2).        
    
readModel :-
    open('problogModel.pl', read, Stream2, [encoding(utf8)]),
    read_stream_to_codes(Stream2, CodesLine),
    string_codes(DataLine, CodesLine),
    close(Stream2),
    asserta(modelContent(DataLine)), !.  

openOutput :- 
     open('problogTotalOut.pl', write, OutputStream, [encoding(utf8)]),
     asserta(outputStream(OutputStream)).

generateNthreads(N) :-
    findall(_, 
        (between(1, N, Value), 
        atomic_concat('problog_', Value, Alias), 
        asserta(availableThread(Alias))), _).

selectNextEvent(Event) :- 
    QueryTriples = [
        ['?ev', rdf:'type', panda:'Event'],
        ['?st', panda:'represents', '?ev'],
        ['?probInput', dc:'subject', '?st'],
        not([
            ['?probComp', dc:'subject', '?ev'],
            ['?probComp', rdf:'type', prov:'Entity'],
            not([['?probComp2', dct:'replaces', '?probComp']]),
            not([
                ['?probComp', prov:'wasGeneratedBy', '?aciv1'],
                ['?aciv1', prov:'used', '?probInput']
                ])
            ])
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?ev', [Event]).

selectNextEvent(Event) :-
    QueryTriples = [
        ['?ev', rdf:'type', panda:'Event'],
        ['?ev', panda:'hasRelatedExperimentalResults', '?res'],
        not([
            ['?probComp', dc:'subject', '?ev'],
            ['?probComp', rdf:'type', prov:'Entity'],
            not([['?probComp2', dct:'replaces', '?probComp']]),
            not([
                ['?probComp', prov:'wasGeneratedBy', '?aciv1'],
                ['?aciv1', prov:'used', '?res']
                ])
            ])
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?ev', [Event]).
    
    
select_read :-
    selectNextEvent(Event),
    gensym('', Counter), atom_number(Counter, No), % No<10, 
    writeln(No),
    queue(Event).

queue(Ev) :-
    once(availableThread(Alias)) 
        -> (retract(availableThread(Alias)), 
            thread_create((single_run(Ev), thread_create(freeWhenDead(Alias), _, [detached(true)])), _, [detached(true), alias(Alias)]));
         queue(Ev).

single_run(Ev) :-
    createInputFile(Ev, InputFile, OutputFile),
    commandProblog(ProblogCommand),
    atomic_list_concat([ProblogCommand, ' -o ', OutputFile, ' ', InputFile], Command),
    shell(Command),
    readOutput(OutputFile, Output),
    outputStream(OutputStream),
    write(OutputStream, Output),
    createUncertaintyInferenceGraph(Ev),
    atomic_list_concat(['cmd /C del ', InputFile, ' ', OutputFile], DelCom),
    shell(DelCom).
    
createInputFile(Ev, InputFile, OutputFile) :-
    uuid(Id),
    atomic_list_concat(['problogInput_', Id, '.pl'], InputFile),
    atomic_list_concat(['problogOutput_', Id, '.pl'], OutputFile),
    open(InputFile, write, Stream, [encoding(utf8)]), !,
    inputData(Ev, Stream),
    close(Stream),
        open(InputFile, read, Stream2, [encoding(utf8)]),
        read_stream_to_codes(Stream2, CodesLine),
        string_codes(DataLine2, CodesLine),
        asserta(addTotalInput(DataLine2)),
        close(Stream2),
    open(InputFile, append, Stream3, [encoding(utf8)]),
    modelContent(DataLine),
    writeln(Stream3, DataLine),
    close(Stream3).
   
inputData(Ev, Stream) :-
    atomic_list_concat(["event('", Ev, "')."], DataLine),
    writeln(Stream, DataLine),
    findall(_, codeRepresentingStatement(Ev, Stream), _).
    
codeRepresentingStatement(Ev, Stream) :-
    representingStatement(Ev, Stat, Value, Doc),
    atomic_list_concat(["representingStatement('", Ev, "', '", Stat, "', ", Value, ", '", Doc, "')."], DataLine),
    writeln(Stream, DataLine),
    findall(_, extractionUncertainty(Stat, Stream), _),
    findall(_, textualUncertainty(Stat, Stream), _),
    findall(_, provenanceUncertainty(Stat, Doc, Stream), _),
    findall(_, experimentUncertainty(Ev, Stream), _).
    
representingStatement(Ev, Stat, Value, Doc) :-  
    QueryTriples = [
        [Ev, panda:'isRepresentedBy', '?stat'],
        ['?stat',  panda:'hasTruthValue', '?value'],
        ['?stat', panda:'isExtractedFrom', '?doc']
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?stat ?value ?doc', [Stat, Value, Doc]).
    
provenanceUncertainty(Stat, Doc, Stream) :-
    QueryTriples = [
        [Stat, uno:'hasProvenanceUncertainty', '?prob'],
        ['?prob',   uno:'hasUncertaintyLevel', '?level']
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?level', [No]),
    atomic_list_concat(["provenanceProb('", Doc, "',", No, ")."], DataLine),
    writeln(Stream, DataLine).    

textualUncertainty(Stat, Stream) :-
    QueryTriples = [
        [Stat, uno:'hasTextualUncertainty', '?prob'],
        ['?prob',   uno:'hasUncertaintyLevel', '?level']
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?level', [Level]),
    atomic_list_concat(["textProb('", Stat, "',", Level, ")."], DataLine),
    writeln(Stream, DataLine).

extractionUncertainty(Stat, Stream) :-
    QueryTriples = [
        [Stat, uno:'hasExtractionAccurracy', '?prob'],
        ['?prob',   uno:'hasUncertaintyLevel', '?level']
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?level', [Level]),
    atomic_list_concat(["extractionProb('", Stat, "',", Level, ")."], DataLine),
    writeln(Stream, DataLine).

experimentUncertainty(Ev, Stream) :-
    QueryTriples = [
        [Ev, panda:'hasRelatedExperimentalResults', '?exp'],
        ['?exp',   panda:'hasTruthValue', '?value']
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?value', [Value]),
    atomic_list_concat(["experimentProb('", Ev, "',", Value, ")."], DataLine),
    writeln(Stream, DataLine).
    

    
%%%%%%%%%%%%%%%%%%%%%%
% Recording results
%%%%%%%%%%%%%%%%%%%%%%

readOutput(OutputFile, Output) :-
    open(OutputFile, read, Stream, [encoding(utf8)]),
    read_stream_to_codes(Stream, CodesFile),
    subtract(CodesFile, [32, 9], NoSpaceOutput),
    string_codes(Output, NoSpaceOutput),
    atomic_list_concat(AnswerList, '\n', Output),
    subtract(AnswerList, [''], OutList),
    findall(_, registerOutput(OutList), _),
    close(Stream).

registerOutput(AnswerList) :-
    member(Answer, AnswerList),
    atomic_list_concat([Ato, Prob], '):', Answer),
    atomic_list_concat([Ato, ')'], Atom),
    atom_number(Prob, ProbNum),
    assertProbabilisticFact(Atom, ProbNum).
    
assertProbabilisticFact(Atom, Prob) :-
    read_term_from_atom(Atom, Term, []),
    Term =.. TermList,
    union(TermList, [Prob], ListArg),
    union([probFact], ListArg, FinalTermList),
    FinalTermOut =..FinalTermList,
    asserta(FinalTermOut).
    
relationType2RDF('negConflict', 'http://purl.bioontology.org/net/brunel/p#NegativeConflict', 'http://purl.bioontology.org/net/brunel/p#hasNegativeConflict').
relationType2RDF('posConflict', 'http://purl.bioontology.org/net/brunel/p#PositiveConflict', 'http://purl.bioontology.org/net/brunel/p#hasPositiveConflict').
relationType2RDF('conflict', 'http://purl.bioontology.org/net/brunel/p#TotalConflict', 'http://purl.bioontology.org/net/brunel/p#hasTotalConflict').
relationType2RDF('negCorroboration', 'http://purl.bioontology.org/net/brunel/p#NegativeCorroboration', 'http://purl.bioontology.org/net/brunel/p#hasNegativeCorroboration').
relationType2RDF('posCorroboration', 'http://purl.bioontology.org/net/brunel/p#PositiveCorroboration', 'http://purl.bioontology.org/net/brunel/p#hasPositiveCorroboration').
relationType2RDF('corroboration', 'http://purl.bioontology.org/net/brunel/p#TotalCorroboration', 'http://purl.bioontology.org/net/brunel/p#hasTotalCorroboration').
relationType2RDF('internalInconsistency', 'http://purl.bioontology.org/net/brunel/p#UncertaintyRelevantToEvidenceInconsistency', 'http://purl.bioontology.org/net/brunel/p#hasInternalInconsistency').
    
resultTriples(Triples) :- 
findall(Triple, (
                probFact(Type, Ev, P),
                relationType2RDF(Type, Class, Property),
                createFreshObject(Class, Uncertainty),
                resultTriple(Ev, Class, Uncertainty, Property, P, Triple)), 
        Triples).
    
resultTriple(Ev, _, Uncertainty, Property, _, [Ev, Property, Uncertainty]).
resultTriple(_, Class, Uncertainty, _, _, [Uncertainty, rdf:'type', Class]).
resultTriple(_, _, Uncertainty, _, P, [Uncertainty, uno:'hasUncertaintyLevel', literal(P)]).
resultTriple(_, _, Uncertainty, _, P, [Uncertainty, rdfs:'label', literal(P)]).


createUncertaintyInferenceGraph(Event) :-
    createFreshObject('uncertainty_inference_graph', Graph),
    createFreshObject('problog_reasoning', Activity),
    date(date(Y, M, D)),
    atomic_list_concat([Y, M, D], '-', Date),
    atomic_list_concat([Graph, '.rdf'], GraphDump),
    atomic_list_concat([Graph, '.graph'], GraphBrowse),
    MetaTriples = [
                [Graph, rdf:'type', void:'Dataset'],
                [Graph, rdf:'type', prov:'Entity'],
                [Graph, dc:'subject', Event],
                [Graph, dc:'creator', literal('Big Mechanism')],
                [Graph, dc:'title', literal('Probabilistic inference results')],
                [Graph, dc:'description', literal('This dataset contains probabilistic annotations generated using ProbLog tool.')],
                [Graph, dc:'date', literal(Date)],
                [Graph, void:'dataDump', GraphDump],
                [Graph, void:'dataBrowse', GraphBrowse],
                [Graph, prov:'wasGeneratedBy', Activity],
                [Activity, rdf:'type', prov:'Activity'],
                [Activity, rdfs:'label', literal('ProbLog reasoning')],
                [Activity, prov:'startedAtTime', literal(Date)],
                [Activity, prov:'endedAtTime', literal(Date)],
                [Activity, prov:'used', 'http://purl.bioontology.org/net/brunel/problog_model.pl']
                ],
    findall([Activity, prov:'used', Used], selectUsedEntitites(Event, Used), UsedTriples),
    findall([Graph, dct:'replaces', Replaced], selectReplaced(Event, Replaced), ReplacedTriples),
    resultTriples(ResultTriples),
    union(UsedTriples, ReplacedTriples, Union1),
    union(ResultTriples, Union1, Union2),
    union(MetaTriples, Union2, TargetTriples),
    sparqlInsertQuery(TargetTriples, Graph).


selectUsedEntitites(Event, Stat) :- 
    QueryTriples = [
                ['?st', panda:'represents', Event],
                ['?prob', dc:'subject', '?st'],
                ['?prob', rdf:'type', prov:'Entity']
                ],
    sparqlSelectQueryGlobal(QueryTriples, '?prob', [Stat]).
    
selectUsedEntitites(Event, Exp) :- 
    QueryTriples = [[Event, panda:'hasRelatedExperimentalResults', '?exp']],
    sparqlSelectQueryGlobal(QueryTriples, '?exp', [Exp]).
    
selectReplaced(Event, Replaced) :-
    QueryTriples = [
                ['?probComp', dc:'subject', Event],
                ['?probComp', rdf:'type', prov:'Entity'],
                not([['?probComp2', dct:'replaces', '?probComp']])
                ],
    sparqlSelectQueryGlobal(QueryTriples, '?probComp', [Replaced]).
