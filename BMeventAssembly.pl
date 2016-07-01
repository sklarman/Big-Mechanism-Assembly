:- use_module(library(gensym)).

:- reset_gensym.

:- ['BMsemWebTools.pl'].

%:- rdf_load('PAF.owl', [graph(pafOnto)]).
%:- rdf_load('P.owl', [graph(pOnto)]).
%:- rdf_load('datasetNactem20160101.rdf', [graph(graph)]).
%:- rdf_load('journals-scimagojr-xlsx.rdf', [graph(graph2)]).
%:- rdf_load('C:/chebi.owl', [graph(chebi)]).

:- prolog_load_context(directory, Dir),  asserta(user:file_search_path(my_home, Dir)).


%% Defining auxiliary predicates

:- dynamic index_graph/1.
:- dynamic card/2.
:- dynamic log_stream/1.
:- dynamic match/3.
:- dynamic matched/1.
:- dynamic registeredEvent/4.
:- rdf_meta aux(r, r, r).
:- rdf_meta createFreshObject(r, r).
:- rdf_meta createIdObject(-, r, -).


%:- asserta(user:file_search_path(index_data, '//acfs4/cssf/csstssk/desktop/biomaterialy/evaluation/machine_with_ihmc_corrected/corrected_cards')).
:- asserta(user:file_search_path(index_data, 'C:/Users/user/Dysk Google/Prolog')).
%:- asserta(user:file_search_path(index_data, 'C:/Users/csstssk/Google Drive/Prolog')).
%:- asserta(user:file_search_path(index_data, 'C:/Users/Administrator/Desktop/Assembly')).

%% top-level control

run :-
    file_search_path(my_home, Dir2), !,
    string_concat(Dir2, '/log_indexCardAssembly.txt', LogFile),
    open(LogFile, write, Stream),
    asserta(log_stream(Stream)), !,
    findall(_, assembleNextFolder, _),
    close(Stream).

assembleNextFolder :- 
    member(Folder, [
        '/IndexTestNactem/*.*'
        %'/SMALL/*.*'
        %'/IHMC/*.*',
        %'/FRIEScards/*.*'
        %'/USC/*/*.*'
        ]),
    nl, write('Collecting index card files from '), write(Folder), writeln('...'),
    filecollector(Folder, FileList), 
    length(FileList, ListLength),
    write(ListLength), writeln(' files collected.'),
    nl, writeln('Assembling index cards...'),
    translate2rdf(FileList).

filecollector(Folder, FileList) :-
    file_search_path(index_data, Dir), 
    string_concat(Dir, Folder, Path),
    writeln(Path),
    expand_file_name(Path, FileList), !.
    
translate2rdf([]).

translate2rdf([FileName|_]) :-
    translateCard(FileName).
    
translate2rdf([_|Rest]) :-
    translate2rdf(Rest).

translateCard(FileName) :- 
    indexCard(Index, FileName),
    file_base_name(FileName, Base), 
    string_concat(BaseName, '.json', Base),
    log_write(BaseName),
    print_count100,
    cardElement(Index, ''), !,
    (createRDF(BaseName) -> (log_writeln('\nSuccess!\n\n'), writeln('Success!'));(log_writeln('\nFail!\n\n'), writeln('Fail!'))),
    retractall(card(_, _)).

%% json index card reader

indexCard(Term, FileName) :- 
    open(FileName, read, IndexCard), 
    json_read(IndexCard, TopTerm),
    TopTerm = json(Term),
    close(IndexCard).

print_count100 :-
    gensym('', Counter), 
    atom_number(Counter, Number), 
    0 is mod(Number, 100) -> writeln(Number); true.

print_count10 :-
    gensym('', Counter), 
    atom_number(Counter, Number), 
    0 is mod(Number, 10) -> writeln(Number); true.

log_write(Content) :-
    log_stream(Stream),
    write(Stream, Content).

log_writeln(Content) :-
    log_stream(Stream),
    writeln(Stream, Content).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% index card content extractor
%  syntax: cardElement(JsonTerm, Key, Value)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cardElement([], _).
cardElement(_ = [], _).
cardElement(_ = '', _).
cardElement(_ = @null, _).

cardElement([First|Rest], Branch) :-
    cardElement(First, Branch),
    cardElement(Rest, Branch).

cardElement(Key = json(SubTerm), Branch) :-
    atomic_list_concat([Branch, Key], '/', XBranch),
    cardElement(SubTerm, XBranch).

cardElement(Key = [FirstTerm|Rest], Branch) :-
    cardElement(Key = FirstTerm, Branch),
    cardElement(Key = Rest, Branch).

cardElement(Key = Value, Branch) :- 
    \+Value = json(_),
    \+is_list(Value),
    \+Value = '',
    \+Value = @null,
    atomic_list_concat([Branch, Key], '/', XBranch),
    assert(card(XBranch, Value)).



%%%%%%%%%%%%%%%%%%%%%%%
%   Top RDF structure creation
%%%%%%%%%%%%%%%%%%%%%%%

createRDF(BaseName) :- 
    nl, write('Processing index card: \t'), writeln(BaseName),
    createIndexCardGraph(GraphURI), 
    createFreshObject(bmpaf:'IndexCard', IndexCard), !,
    createIdObject(Event, bmpaf:'Event', '/extracted_information', GroundingProbList),
    createFreshObject(bmpaf:'Statement', Statement),
    createIdObject(Submitter, bmpaf:'Submitter', '', []),
    createIdObject(Article, bmpaf:'JournalArticle', '', []),
    retriveKeyValue(readingStarted, StartTime),
    retriveKeyValue(readingStopped, StopTime),
    retriveKeyValue(truthValue, TruthValue),
    retriveKeyValue(readerType, ReaderType),
    retriveKeyValue(submitter, SubmitLab),
    retriveKeyValue(pmcId, PMCid),
    event_graph(EventGraph),
    getLabel(Event, EventGraph, EventLabel),
    atomic_list_concat(['{', EventLabel, '} ', TruthValue, ' in ', PMCid, ' (by ', SubmitLab ,')'], StatLabel),
    BasicTriples = [
        [IndexCard, rdf:'type', bmpaf:'IndexCard'],
        [IndexCard, bmpaf:'hasStartTime', literal(StartTime)],
        [IndexCard, bmpaf:'hasStopTime', literal(StopTime)],
        [IndexCard, bmpaf:'hasIndexCardId', literal(BaseName)],
        [IndexCard, bmpaf:'hasAnnotator', ReaderType],
        [IndexCard, bmpaf:'hasSubmitter', Submitter],
        [IndexCard, bmpaf:'containsStatement', Statement],
        [Statement, rdf:'type', bmpaf:'Statement'],
        [Statement, bmpaf:'containedIn', IndexCard],
        [Statement, rdf:'type', prov:'Entity'],
        [Statement, rdfs:'label', literal(StatLabel)],
        [Statement, bmpaf:'hasTruthValue', literal(TruthValue)],
        [Statement, bmpaf:'represents', Event],
        [Event, bmpaf:'isRepresentedBy', Statement],
        [Statement, bmpaf:'isExtractedFrom', Article]
        ],
    findall([Statement, bmpaf:'hasTextualEvidence', literal(Evidence)], card('/evidence', Evidence), EvidenceTriples),
    union(BasicTriples, EvidenceTriples, TargetTriples),
    sparqlInsertQuery(TargetTriples, GraphURI),
    write('Created new statement: \t'), writeln(StatLabel),
    log_write('\nCreated: \t'), log_writeln(StatLabel),
    createInputProbabilityGraph(Statement, ProbGraph),
    writeln('Creating probability input graph...'),
    findall(_, probabilityInput(Statement, ProbGraph), _),
    findall(_, probabilityInput(Statement, GroundingProbList, ProbGraph), _).

createIndexCardGraph(Graph) :-
    index_graph(Graph) -> true;     
            (date(date(Y, M, D)),
            atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/index_card_graph_', Y, M, D], Graph), 
            atomic_list_concat([Y, M, D], '-', Date),
            TargetTriples = [
                [Graph, rdf:'type', void:'Dataset'],
                [Graph, dc:'creator', literal('Big Mechanism')],
                [Graph, dc:'title', literal('Textual evidence extracted from literature')],
                [Graph, dc:'description', literal('Data about statements extracted automatically from scientific literature and reported in a collection of index card submitted to the system.')],
                [Graph, dc:'date', literal(Date)]
                ],
            sparqlInsertQuery(TargetTriples, Graph),
            asserta(index_graph(Graph))).

createInputProbabilityGraph(Statement, Graph) :-
    createFreshObject('probability_input_graph', Graph),
    date(date(Y, M, D)),
    atomic_list_concat([Y, M, D], '-', Date),
    atomic_list_concat([Graph, '.rdf'], GraphDump),
    atomic_list_concat([Graph, '.graph'], GraphBrowse),
    TargetTriples = [
                [Graph, rdf:'type', void:'Dataset'],
                [Graph, rdf:'type', prov:'Entity'],
                [Graph, dc:'subject', Statement],
                [Graph, dc:'creator', literal('Big Mechanism')],
                [Graph, dc:'title', literal('Prior probabilities')],
                [Graph, dc:'description', literal('Data about prior probabilities associated with a single statement.')],
                [Graph, dc:'date', literal(Date)],
                [Graph, void:'dataDump', GraphDump],
                [Graph, void:'dataBrowse', GraphBrowse]
                ],
            sparqlInsertQuery(TargetTriples, Graph).
    

%%%%%%%%%%%%%%%%%%%%%%%
%   Entity creation
%%%%%%%%%%%%%%%%%%%%%%%

createIdObject(Event, bmpaf:'Event', Path, GroundingProbList) :-
    retriveKeyValue(interactionType, Path, EvTypURI),
    \+EvTypURI=bmpaf:'AddsModification',
    \+EvTypURI=bmpaf:'InhibitsModification',
    event_graph(Graph),
    (retriveKeyValue(participantAtype, Path) -> 
        (atomic_list_concat([Path, '/participant_a'], PartApath),
        createIdObject(ParticipantA, bmpaf:'Participant', PartApath, GroundingProbListA), 
        TripleA = [['?x', bmpaf:'hasParticipantA', ParticipantA]],
        getLabel(ParticipantA, Graph, LabelA)); 
        (GroundingProbListA=[], 
         TripleA =[not([['?x', bmpaf:'hasParticipantA', '?y']])],
         LabelA='-')),
    (retriveKeyValue(participantBtype, Path) -> 
        (atomic_list_concat([Path, '/participant_b'], PartBpath),
        createIdObject(ParticipantB, bmpaf:'Participant', PartBpath, GroundingProbListB), 
        TripleB = [['?x', bmpaf:'hasParticipantB', ParticipantB]],
        getLabel(ParticipantB, Graph, LabelB)); 
        (GroundingProbListB=[], 
        TripleB =[not([['?x', bmpaf:'hasParticipantB', '?z']])],
        LabelB='-')),
    BasicTriples = [
        ['?x', rdf:'type', EvTypURI]],
    union(TripleA, TripleB, PartTriples),
    \+PartTriples = [],
    union(BasicTriples, PartTriples, EventTriples),
    (sparqlSelectQuery(EventTriples, Graph, Tuple) -> 
        (Tuple=[Event], log_write('\nMatched '), log_writeln(Event), writeln('Matched existing event...'));
        (createFreshObject(bmpaf:'Event', Event),
        paf_graph(PafGraph),
        getLabel(EvTypURI, PafGraph, EventTypeLabel),
        atomic_list_concat([LabelA, EventTypeLabel, LabelB], ' ', EventLabel),
        log_write('\nCreated '), log_writeln(EventLabel),
        write('Created new event: \t'), writeln(EventLabel),
        LabelTriple=[[Event, rdfs:'label', literal(EventLabel)]],
        substitute('?x', Event, EventTriples, EventTriplesSub),
        union(LabelTriple, EventTriplesSub, AllEventTriples),
        sparqlInsertQuery(AllEventTriples, Graph))),
    union(GroundingProbListA, GroundingProbListB, GroundingProbList).

createIdObject(Participant, bmpaf:'Participant', Path, GroundingProbList) :-
    retriveKeyValue(entityType, Path, EntType),
    createIdObject(Participant, EntType, Path, GroundingProbList), !.   
    
createIdObject(SimpleURI, SimpleType, Path, GroundingProbList) :-
    member(SimpleType, [bmpaf:'Protein', bmpaf:'Chemical', bmpaf:'Gene']), !,
    retriveKeyValue(entityId, Path, EntId),
   % retriveKeyValue(entityText, Path, EntText),
    BasicTriples = [
        ['?x', bmpaf:'hasEntityId', literal(EntId)]
        ],
    event_graph(Graph),
    (sparqlSelectQuery(BasicTriples, Graph, Tuple) -> 
        (Tuple=[SimpleURI],
        log_write('\nMatched '), log_writeln(EntId),
        write('Matched existing entity: \t'), writeln(EntId),
        ExtraTargetTriples=[]);
        (createIdURI(simple, EntId, SimpleURI),
        (identifierResolution(EntId, Name) ->       
        ExtraTargetTriples=[
            [SimpleURI, bmpaf:'hasEntityId', literal(EntId)],
            [SimpleURI, bmpaf:'hasEntityName', literal(Name)],
            [SimpleURI, rdfs:'label', literal(Name)]
        ];
        ExtraTargetTriples=[
            [SimpleURI, bmpaf:'hasEntityId', literal(EntId)]
            ])), 
        BasicTargetTriples = [
         %   [SimpleURI, bmpaf:'hasEntityText', literal(EntText)],
            [SimpleURI, rdf:'type', SimpleType]
            ],
        union(BasicTargetTriples, ExtraTargetTriples, TargetTriples),
        sparqlInsertQuery(TargetTriples, Graph),
        log_write('\nCreated '), log_writeln(EntId),
        write('Created new entity: \t'), writeln(EntId)),
    GroundingProbList=[[SimpleURI, 1]].
    
createIdObject(Submitter, bmpaf:'Submitter', '', []) :-
    retriveKeyValue(submitter, SubmitterName),
    BasicTriples = [
        ['?x', bmpaf:'hasSubmitterId', literal(SubmitterName)]
        ],
    submitter_graph(Graph),
    (sparqlSelectQuery(BasicTriples, Graph, Tuple) -> 
        Tuple=[Submitter];
        (createFreshObject(bmpaf:'Submitter', Submitter),
        TargetTriples = [
        [Submitter, bmpaf:'hasSubmitterId', literal(SubmitterName)],
        [Submitter, rdf:'type', bmpaf:'Submitter'],
        [Submitter, rdfs:'label', literal(SubmitterName)]
        ],
        sparqlInsertQuery(TargetTriples, Graph)
        )).

createIdObject(Article, bmpaf:'JournalArticle', '', []) :-
    retriveKeyValue(pmcId, PmcId),
    BasicTriples = [
        ['?x', bmpaf:'hasPMCId', literal(PmcId)]
        ],
    source_graph(Graph),
    (sparqlSelectQuery(BasicTriples, Graph, Tuple) -> 
        Tuple=[Article];
        (
        createIdURI(article, PmcId, Article),
        pubmedJournalTitleYear(PmcId, ISSN, Title, Year),
        createIdObject(Journal, bmpaf:'Journal', ISSN, []),
        TargetTriples = [
            [Article, bmpaf:'hasPMCId', literal(PmcId)],
            [Article, bmpaf:'hasTitle', literal(Title)],
            [Article, rdfs:'label', literal(Title)],
            [Article, rdf:'type', bmpaf:'JournalArticle'],
            [Article, bmpaf:'publishedIn', Journal],
            [Article, bmpaf:'hasPublicationYear', Year]
        ],
        sparqlInsertQuery(TargetTriples, Graph)
        )).

createIdObject(Journal, bmpaf:'Journal', ISSN, []) :-
    BasicTriples = [
        ['?x', bmpaf:'hasISSN', literal(ISSN)]
        ],
    journal_graph(Graph),
    (sparqlSelectQuery(BasicTriples, Graph, Tuple) -> 
        Tuple=[Journal];
        (createFreshObject(bmpaf:'Journal', Journal),
        TargetTriples = [
        [Journal, rdf:'type', bmpaf:'Journal'],
        [Journal, bmpaf:'hasISSN', literal(ISSN)]
        ],
        sparqlInsertQuery(TargetTriples, Graph))).
        
pubmedJournalTitleYear(PmcId, ISSN, Title, Year) :- 
    atomic_list_concat(['http://www.ncbi.nlm.nih.gov/pmc/utils/ctxp/?ids=', PmcId, '&report=citeproc&format=json'], Query),
    (http_open(Query, In, []) ->
    (json_read(In, json(X)), 
    member('ISSN'=ISSNproper, X), !,
    atomic_list_concat(List, '-', ISSNproper),
    atomic_list_concat(List, ISSN),
    member('title'=Title, X), !,
    member(issued=json(['date-parts'=[[Year | _]]]), X), !); (ISSN='', Year='')).
    
    

%%%%%%%%%%%%%%%%%%%%%%%
%  Info retrieval from the card and mapping to PAF
%%%%%%%%%%%%%%%%%%%%


getLabel(EntityURI, Graph, Label) :-
    sparqlSelectQuery([[EntityURI, rdfs:'label', '?x']], Graph, Tuple) -> 
        Tuple=[Label];Label='?'.

retriveKeyValue(pmcId, PmcId) :-
    card('/pmc_id', PmcIdString),
    upperCase(PmcIdString, PmcIdUpper),
    normalize_space(atom(PmcId), PmcIdUpper).

retriveKeyValue(readingStarted, StartTime) :-
    card('/reading_started', StartTime).
    
retriveKeyValue(readingStopped, StopTime) :-
    card('/reading_complete', StopTime).
    
retriveKeyValue(submitter, Submitter) :-
    card('/submitter', SubmitterString),
    upperCase(SubmitterString, SubmitterUpper),
    normalize_space(atom(Submitter), SubmitterUpper).
    
retriveKeyValue(readerType, ReaderTypeURI)  :-
    (card('/reader_type', ReaderType) -> 
        readerType(ReaderType, ReaderTypeURI);
        ReaderTypeURI = bmpaf:'Annotator').

retriveKeyValue(truthValue, TruthValue) :-
    card('/extracted_information/negative_information', NegInf), 
    negation(NegInf, TruthValue).

retriveKeyValue(participantAtype, Path) :-
    atomic_list_concat([Path, '/participant_a/entity_type'], Key),
    card(Key, Value), 
    participantType(Value, _), !.

retriveKeyValue(participantBtype, Path) :-
    atomic_list_concat([Path, '/participant_b/entity_type'], Key),
    card(Key, Value),
    participantType(Value, _), !.

retriveKeyValue(extractionAccurracy, Accurracy) :-
    card('/meta/confidence', Accurracy).

retriveKeyValue(textualUncertainty, Value) :-  
    card('/meta/speculated_information', SpecInf),
    speculated(SpecInf, Value).
    
retriveKeyValue(interactionType, Path, IntType) :-
    atomic_list_concat([Path, '/interaction_type'], Key),
    card(Key, Value),
    interactionType(Value, IntType).
    
retriveKeyValue(modificationType, Path, ModType) :- 
    atomic_list_concat([Path, '/modifications/modification_type'], Key),
    card(Key, Value),
    modificationType(Value, ModType).

retriveKeyValue(entityType, Path, EntType) :-
    atomic_list_concat([Path, '/entity_type'], Key),
    card(Key, Value),
    participantType(Value, EntType).
    
retriveKeyValue(entityId, Path, EntId) :-
    atomic_list_concat([Path, '/identifier'], Key),
    card(Key, EntIdString),
    upperCase(EntIdString, EntIDupper),
    normalize_space(atom(EntId), EntIDupper).
    
retriveKeyValue(entityText, Path, EntText) :-
    atomic_list_concat([Path, '/entity_text'], Key),
    card(Key, EntText).
    

    
participantType('protein', bmpaf:'Protein').
participantType('chemical', bmpaf:'Chemical').
participantType('protein_family', bmpaf:'ProteinFamily').
participantType('gene', bmpaf:'Gene').
participantType('complex', bmpaf:'Complex').
participantType('event', bmpaf:'Event').
    
interactionType('binds', bmpaf:'Binding').
interactionType('increases', bmpaf:'PositiveRegulation').
interactionType('decreases', bmpaf:'NegativeRegulation').
interactionType('translocates', bmpaf:'Translocation').
interactionType('increases_activity', bmpaf:'IncreasesActivity').
interactionType('decreases_activity', bmpaf:'DecreasesActivity').
interactionType('gene_expression', bmpaf:'GeneExpression').
interactionType('adds_modification', bmpaf:'AddsModification').
interactionType('inhibits_modification', bmpaf:'InhibitsModification').

modificationType('phosphorylation', bmpaf:'Phosphorylation').
modificationType('acetylation', bmpaf:'Acetylation').
modificationType('farnesylation', bmpaf:'Farnesylation').
modificationType('glycosylation', bmpaf:'Glycosylation').
modificationType('hydroxylation', bmpaf:'Hydroxylation').
modificationType('methylation', bmpaf:'Methylation').
modificationType('ribosylation', bmpaf:'Ribosylation').
modificationType('sumoylation', bmpaf:'Sumoylation').
modificationType('ubiquitination', bmpaf:'Ubiquitination').

readerType('human', bmpaf:'Human'). 
readerType('machine', bmpaf:'Computer').
readerType(ReaderType, bmpaf:'HumanComputer') :-
     member(ReaderType, ['human_machine', 'human-machine']).

negation(@false, true).
negation(@true, false).

speculated(@false, 'certain').
speculated(@true, 'uncertain').

%%%%%%%%%%%%%%%%%%%%%%%
%   Input probabilities recording
%%%%%%%%%%%%%%%%%%%%

probabilityInput(Statement, Graph) :- 
    retriveKeyValue(textualUncertainty, Value),
    createFreshObject(bmp:'ProbabilityRelevantToTextualUncertainty', Subject),
    TargetTriples = [
        [Statement, bmp:'hasTextualProbability', Subject],
        [Subject, rdf:'type', bmp:'ProbabilityRelevantToTextualUncertainty'],
        [Subject, bmp:'hasProbabilityLevel', literal(Value)],
        [Subject, rdfs:'label', literal(Value)]
        ],
        sparqlInsertQuery(TargetTriples, Graph).

probabilityInput(Statement, Graph) :- 
    retriveKeyValue(extractionAccurracy, Accurracy),
    createFreshObject(bmp:'AccuracyOfExtractionFromText', Subject),
    TargetTriples = [
        [Statement, bmp:'hasExtractionAccurracy', Subject],
        [Subject, rdf:'type', bmp:'AccuracyOfExtractionFromText'],
        [Subject, bmp:'hasProbabilityLevel', literal(Accurracy)],
        [Subject, rdfs:'label', literal(Accurracy)]
        ],
        sparqlInsertQuery(TargetTriples, Graph).


probabilityInput(Statement, Graph) :- 
    QueryTriples = [
        [Statement, bmpaf:'isExtractedFrom', '?x'],
        ['?x', bmpaf:'publishedIn', '?y'],
        ['?y', bmpaf:'hasSJRscore', '?s']
        ],
    sparqlSelectQueryGlobal(QueryTriples, '?s', [Score]),
    atom_number(Score, ScoreNumber),
    Prob is sqrt(round((ScoreNumber / 10) * 1000) / 1000),
    createFreshObject(bmp:'ProbabilityRelevantToDocumentProvenance', Subject),
    TargetTriples = [
        [Statement, bmp:'hasProvenanceProbability', Subject],
        [Subject, rdf:'type', bmp:'ProbabilityRelevantToDocumentProvenance'],
        [Subject, bmp:'hasProbabilityLevel', literal(Prob)],
        [Subject, rdfs:'label', literal(Prob)]
        ],
        sparqlInsertQuery(TargetTriples, Graph).

probabilityInput(Statement, GroundingProbList, Graph) :-
    findall(P, member([_, P], GroundingProbList), PList),
    listProduct(PList, Prob),
    createFreshObject(bmp:'ProbabilityRelevantToEntityGrounding', Subject),
    TargetTriples = [
        [Statement, bmp:'hasGroundingProbability', Subject],
        [Subject, rdf:'type', bmp:'ProbabilityRelevantToEntityGrounding'],
        [Subject, bmp:'hasProbabilityLevel', literal(Prob)],
        [Subject, rdfs:'label', literal(Prob)]
        ],
        sparqlInsertQuery(TargetTriples, Graph).

%%%%%%%%%%%%%%%%%%%%
% generic predicates
%%%%%%%%%%%%%%%%%%%

upperCase(Value, ValUpper) :-
    string_upper(Value, ValueUpper),
    atom_string(ValUpper, ValueUpper).

listProduct([H], H) :- !.

listProduct([H|Rest], Product) :-
    listProduct(Rest, PartialProduct),
    Product is PartialProduct*H.
    
%%%%%%%%%%%%%%%%%%%%%
% URI generation
%%%%%%%%%%%%%%%%%%%%%

createIdURI(simple, Id, IdURI) :-
    string_concat('CHEBI:', Number, Id),
    string_concat('http://purl.obolibrary.org/obo/CHEBI_', Number, IdURI).

createIdURI(simple, Id, IdURI) :-
    string_concat('UNIPROT:', Number, Id),
    string_concat('http://purl.uniprot.org/uniprot/', Number, IdURI).

createIdURI(article, Id, IdURI) :-
    string_concat('http://www.ncbi.nlm.nih.gov/pmc/articles/', Id, IdURI).


%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPARQL-based Id resolution
%%%%%%%%%%%%%%%%%%%%%%%%%

sparql_params('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> <http://purl.uniprot.org/core/mnemonic> ?x}', 'sparql.uniprot.org', '/sparql/').
%sparql_params('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> rdfs:label ?x}', 'sparql.uniprot.org', '/sparql/').
sparql_params('CHEBI:', 'SELECT ?x WHERE {<http://purl.obolibrary.org/obo/CHEBI_', '> rdfs:label ?x}', 'chebi.bio2rdf.org', '/sparql').
sparql_params('HGNC:', 'SELECT ?x WHERE { <http://bio2rdf.org/hgnc:', '> <http://bio2rdf.org/hgnc_vocabulary:approved-symbol> ?x}', 'hgnc.bio2rdf.org', '/sparql').

identifierResolution(ProtID, Name) :-
    upperCase(ProtID, ProtIDupper),
    sparql_params(Prefix, QueryFront, QueryBack, Host, Path),
    string_concat(Prefix, Number, ProtIDupper),
    atomic_list_concat([QueryFront, Number, QueryBack], Query),
    sparql_query(Query, row(Literal), [host(Host), path(Path), status_code(_)]),
    rdf_literal_value(Literal, Name), !.


