%% Program pre-processing an OWL/RDF file for the use in ProbLog
%%
%% The four type of probabilities over BEL statements are assigned randomly
%%
%%


:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(readutil)).

:- use_module(library(gensym)).
:- reset_gensym.
:- dynamic probFact/3.
:- dynamic modelContent/1.
:- dynamic outputStream/1.
:- dynamic availableThread/1.
:- dynamic addTotalInput/1.

:- rdf_register_prefix(mod, 'http://purl.org/pc2/7/#').
:- rdf_register_prefix(bm, 'http://purl.bioontology.org/net/brunel/bm/').
:- rdf_register_prefix(bmpaf, 'http://purl.bioontology.org/net/brunel/paf#').
:- rdf_register_prefix(bmp, 'http://purl.bioontology.org/net/brunel/p#').
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov-o#').
:- prolog_load_context(directory, Dir),  asserta(user:file_search_path(my_home, Dir)).


%% Defining an auxiliary predicate

commandProblog('cmd /c "C:/Users/Szymon/Google Drive/problog2.1/problog-cli.py"').
%commandProblog('"C:/Program Files (x86)/Python 3.5/python.exe" "C:/Users/csstssk/Google Drive/problog2.1/problog-cli.py"').

%inputFile('dataset20160101assembled.rdf').
inputFile('dataset20162902assembled.rdf').
%inputFile('experiments20160315.rdf').


loadAllData :- 
    findall(_, (inputFile(X),
                string_concat(Name, '.rdf', X), 
                atom_string(N, Name),
                rdf_load(X, [graph(N)])), _),
    get_time(TimeStamp), 
    stamp_date_time(TimeStamp, date(Y, M, D, H, Min, _, _, _, _), 0), 
    atomic_list_concat(['probabilities_', Y, M, D, H, Min], Target),
    asserta(targetGraph(Target)).

:- loadAllData.

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
    close(Stream2),
    writeln('Encoding in the RDF output...'),
    findall(_, encodeSolutions, _),
    targetGraph(Graph), !,
    addMetaData(Graph),
    saveGraph(Graph).        
    
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

select_read :-
    rdf(Ev, rdf:'type', bmpaf:'Event'),
    rdf(Ev, bmpaf:'hasParticipantA', _),
    rdf(Ev, bmpaf:'hasParticipantB', _),
    gensym('', Counter), atom_number(Counter, No), % No<10, 
    writeln(No),
    queue(Ev).

eventDebug(Ev, InputFile) :-
    rdf(Ev, rdf:'type', bmpaf:'Event'),
    rdf(Ev, bmpaf:'hasParticipantA', _),
    rdf(Ev, bmpaf:'hasParticipantB', _),
    createInputFile(Ev, InputFile, _).

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
    findall(_, codeGraphMatch(Ev, Stream), _),
    findall(_, codeRepresentingStatement(Ev, Stream), _).
    
codeGraphMatch(Ev, Stream) :-
    graphMatch(Ev, Gr, Value),
    atomic_list_concat([Value, "::modelMatchProb('", Ev, "', '", Gr, "')."], DataLine),
    writeln(Stream, DataLine).
   
%codeGraphMatch(Ev, Stream) :-
%    \+graphMatch(Ev, _, _),
%    atomic_list_concat([0, "::modelMatchProb('", Ev, "')."], DataLine),
%    writeln(Stream, DataLine).   

graphMatch(Ev, Gr, Value) :-    
    rdf(Ev, bm:'hasMatch', Gr),
    rdf(Gr, bmp:'hasAssemblyProbability', Prob),
    rdf(Prob, bmp:'hasProbabilityLevel', ProbLiteral),
    rdf_literal_value(ProbLiteral, ProbValue),
    Value is (round(ProbValue * 10000) / 10000).
    
codeRepresentingStatement(Ev, Stream) :-
    representingStatement(Ev, Stat, Value, Doc),
    atomic_list_concat(["representingStatement('", Ev, "', '", Stat, "', ", Value, ", '", Doc, "')."], DataLine),
    findall(_, statementProbability(Stat, Stream), _),
    findall(_, documentProbability(Doc, Stream), _),
    findall(_, experimentProbability(Ev, Stream), _),
    writeln(Stream, DataLine).

%codeRepresentingStatement(Ev, Stream) :-
%    \+representingStatement(Ev, _, 'true', _),
%    atomic_list_concat(["0::positiveSupport('", Ev, "')."], DataLine),
%    writeln(Stream, DataLine).

%codeRepresentingStatement(Ev, Stream) :-
%    \+representingStatement(Ev, _, 'false', _),
%    atomic_list_concat(["0::negativeSupport('", Ev, "')."], DataLine),
%    writeln(Stream, DataLine).
    
representingStatement(Ev, Stat, Value, Article) :-  
    rdf(Ev, bmpaf:'isRepresentedBy', Stat),
    rdf(Stat, bmpaf:'hasTruthValue', TruthLit),
    rdf(Stat, bmpaf:'isExtractedFrom', Article),
    rdf_literal_value(TruthLit, Value).
    
documentProbability(Doc, Stream) :-
    rdf(Doc, bmp:'hasProvenanceProbability', Prob),
    rdf(Prob, bmp:'hasProbabilityLevel', literal(type('http://www.w3.org/2001/XMLSchema#decimal', No))),
    atomic_list_concat(["provenanceProb('", Doc, "',", No, ")."], DataLine),
    %atomic_list_concat([No,"::provenanceProb('", Doc, "')."], DataLine),
    writeln(Stream, DataLine).    

%documentProbability(Doc, Stream) :-
%    \+rdf(Doc, bmp:'hasProvenanceProbability', _),
%    atomic_list_concat([0.1,"::provenanceProb('", Doc, "')."], DataLine),
%    writeln(Stream, DataLine).    
    
%documentProbability(Doc, Stream) :-
%    atomic_list_concat([1,"::provenanceWeight('", Doc, "')."], DataLine),
%    writeln(Stream, DataLine).    

statementProbability(Stat, Stream) :-
    rdf(Stat, bmp:'hasTextualProbability', Prob),
    rdf(Prob, bmp:'hasProbabilityLevel', ProbLiteral),
    rdf_literal_value(ProbLiteral, Value),
    %rdf_literal_value(ProbLiteral, 'certain'),
    atomic_list_concat(["textProb('", Stat, "',", Value, ")."], DataLine),
    %atomic_list_concat([1,"::textProb('", Stat, "')."], DataLine),
    writeln(Stream, DataLine).

%statementProbability(Stat, Stream) :-
%    rdf(Stat, bmp:'hasTextualProbability', Prob),
%    rdf(Prob, bmp:'hasProbabilityLevel', ProbLiteral),
%    rdf_literal_value(ProbLiteral, 'uncertain'),
%    atomic_list_concat([0.8,"::textProb('", Stat, "')."], DataLine),
%    writeln(Stream, DataLine).    

%statementProbability(Stat, Stream) :-
%    \+rdf(Stat, bmp:'hasTextualProbability', _),
%    atomic_list_concat([1,"::textProb('", Stat, "')."], DataLine),
%    writeln(Stream, DataLine).    

statementProbability(Stat, Stream) :-
    rdf(Stat, bmp:'hasExtractionAccurracy', Prob),
    rdf(Prob, bmp:'hasProbabilityLevel', literal(type('http://www.w3.org/2001/XMLSchema#decimal', No))),
    atomic_list_concat(["extractionProb('", Stat, "',", No, ")."], DataLine),
    %atomic_list_concat([No,"::extractionProb('", Stat, "')."], DataLine),
    writeln(Stream, DataLine).    

%statementProbability(Stat, Stream) :-
%    \+rdf(Stat, bmp:'hasExtractionAccurracy', _),
%    atomic_list_concat([0.6,"::extractionProb('", Stat, "')."], DataLine),
%    writeln(Stream, DataLine).    

%statementProbability(Stat, Stream) :-
%    atomic_list_concat([1,"::textWeight('", Stat, "')."], DataLine),
%    writeln(Stream, DataLine).     

%statementProbability(Stat, Stream) :-
%    atomic_list_concat([1,"::extractionWeight('", Stat, "')."], DataLine),
%    writeln(Stream, DataLine).   
    
experimentProbability(Ev, Stream) :-
    rdf(Ev, bmpaf:'hasRelatedExperimentalResults', Exp),
    rdf(Exp, bmpaf:'hasTruthValue', ExpVal),
    rdf_literal_value(ExpVal, Value),
    %rdf_literal_value(ExpVal, 'true'),
    atomic_list_concat(["experimentProb('", Ev, "',", Value, ")."], DataLine),
    %atomic_list_concat([0.95,"::experimentProb('", Ev, "')."], DataLine),
    writeln(Stream, DataLine).

%experimentProbability(Ev, Stream) :-
%    rdf(Ev, bmpaf:'hasExperimentValue', ExpVal),
%    rdf_literal_value(ExpVal, 'false'),
%    atomic_list_concat([0.2,"::experimentProb('", Ev, "')."], DataLine),
%    writeln(Stream, DataLine). 

%experimentProbability(Ev, Stream) :-
%    \+rdf(Ev, bmpaf:'hasExperimentValue', _),
%    atomic_list_concat([0.5,"::experimentProb('", Ev, "')."], DataLine),
%    writeln(Stream, DataLine). 

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

orderSolutions(Dim, SortedList) :- 
    findall([P, Ev], probFact(Dim, Ev, P), List),
    sort(0,  @>=, List,  SortedList).

%topK(Dim, K, Solution) :-
%    orderSolutions(Dim, SortedList),
%    nth1(N, SortedList, Solution),
%    N =< K.

top(Dim, Solution) :-
    orderSolutions(Dim, SortedList),
    nth1(_, SortedList, Solution).
    
%%%%%%%%%%%%%%%%%%%%%%

encodeSolutions :- 
    probFact(Type, Ev, P),
    relationType2RDF(Type, Pred, Prop),
    createClassedObject('probability', Pred, Probability),
    writeInTargetGraph(Ev, Prop, Probability),
    insertDataValue(Probability, 'http://purl.bioontology.org/net/brunel/p#hasProbabilityLevel', P),
    insertDataValue(Probability, rdfs:'label', P).
    
    
relationType2RDF('negConflict', 'http://purl.bioontology.org/net/brunel/p#NegativeConflict', 'http://purl.bioontology.org/net/brunel/p#hasNegativeConflict').
relationType2RDF('posConflict', 'http://purl.bioontology.org/net/brunel/p#PositiveConflict', 'http://purl.bioontology.org/net/brunel/p#hasPositiveConflict').
relationType2RDF('conflict', 'http://purl.bioontology.org/net/brunel/p#TotalConflict', 'http://purl.bioontology.org/net/brunel/p#hasTotalConflict').
relationType2RDF('negCorroboration', 'http://purl.bioontology.org/net/brunel/p#NegativeCorroboration', 'http://purl.bioontology.org/net/brunel/p#hasNegativeCorroboration').
relationType2RDF('posCorroboration', 'http://purl.bioontology.org/net/brunel/p#PositiveCorroboration', 'http://purl.bioontology.org/net/brunel/p#hasPositiveCorroboration').
relationType2RDF('corroboration', 'http://purl.bioontology.org/net/brunel/p#TotalCorroboration', 'http://purl.bioontology.org/net/brunel/p#hasTotalCorroboration').
relationType2RDF('internalInconsistency', 'http://purl.bioontology.org/net/brunel/p#ProbabilityRelevantToEvidenceInconsistency', 'http://purl.bioontology.org/net/brunel/p#hasInternalInconsistency').
    
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insertDataValue(Subject, Predicate, Value) :-
    makeLiteral(Value, Literal),
    writeInTargetGraph(Subject, Predicate, Literal).   
    
makeLiteral(@false, literal(lang(en, 'false'))) :- !.
makeLiteral(@true, literal(lang(en, 'true'))) :- !.
makeLiteral(X, literal(type('http://www.w3.org/2001/XMLSchema#decimal', X))) :- 
    number(X), !.
makeLiteral(Date, literal(Date)) :-
    atomic_list_concat([_, _, _], '-', Date), !.
makeLiteral(Value, literal(lang(en, Value))) :- !.

createClassedObject(UriBase, Class, Subject) :- !,
    createFreshObject(UriBase, Subject),
    writeInTargetGraph(Subject, rdf:'type', Class).
    
createFreshObject(UriBase, Subject) :-
    uuid(U), !, 
    atomic_list_concat([UriBase, '_', U], Subject).
    

writeInTargetGraph(X, Y, Z) :-
    targetGraph(Graph),
    rdf_global_id(X, Xg),
    rdf_global_id(Y, Yg),
    rdf_global_id(Z, Zg),
    rdf_assert(Xg, Yg, Zg, Graph).

%%%%%%%

saveGraph(Graph) :- 
    file_search_path(my_home, Dir),
    atomic_list_concat([Dir, '/', Graph, '.rdf'], OutputPath),
    rdf_save(OutputPath, [graph(Graph), encoding(utf8)]), !.

addMetaData(Graph) :-
    atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/', Graph], GraphURI),
    rdf_assert(GraphURI, rdf:'type', void:'Dataset', Graph),
    rdf_assert(GraphURI, rdf:'type', prov:'Entity', Graph),
    insertDataValue(GraphURI, dc:'creator', 'Szymon Klarman'),
    get_time(TimeStamp), 
    stamp_date_time(TimeStamp, 
    date(Y, M, D, H, Min, _, _, _, _), 0), 
    atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/problogInference_', Y, M, D, H, Min], ActivityURI),
    rdf_assert(GraphURI, prov:'wasGeneratedBy', ActivityURI, Graph),
    rdf_assert(ActivityURI, rdf:'type', prov:'Activity', Graph),
    forall(inputFile(X), 
        (string_concat(Name, '.rdf', X), 
        atom_string(N, Name),
        atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/', N], UsedGraph),
        rdf_assert(ActivityURI, prov:'used', UsedGraph, Graph))),
    date(date(Y, M, D)), atomic_list_concat([Y, M, D], '-', Date),
    insertDataValue(ActivityURI, prov:'startedAtTime', Date),
    insertDataValue(ActivityURI, prov:'endedAtTime', Date),
    insertDataValue(GraphURI, dc:'title', 'Probabilistic inference results'),
    insertDataValue(GraphURI, dc:'description', 'This dataset contains probabilistic annotations generated using ProbLog tool.'),
    insertDataValue(GraphURI, dc:'date', Date).
