%% Program pre-processing an OWL/RDF file for the use in ProbLog
%%
%%


:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(http/json)).
:- use_module(library(http/http_digest)).
:- use_module(library(http/http_open)).
:- use_module(library(gensym)).

:- rdf_reset_db.
:- reset_gensym.

%:- rdf_load('PAF.owl', [graph(pafOnto)]).
%:- rdf_load('P.owl', [graph(pOnto)]).
%:- rdf_load('datasetNactem20160101.rdf', [graph(graph)]).
%:- rdf_load('journals-scimagojr-xlsx.rdf', [graph(graph2)]).
%:- rdf_load('C:/chebi.owl', [graph(chebi)]).

%:- rdf_register_prefix(nov, 'http://localhost:3333/').
%:- rdf_register_prefix(mod, 'http://purl.org/pc2/7/#').
:- rdf_register_prefix(unprot, 'http://identifiers.org/uniprot/').
:- rdf_register_prefix(bm, 'http://purl.bioontology.org/net/brunel/bm/').
:- rdf_register_prefix(bmpaf, 'http://purl.bioontology.org/net/brunel/paf#').
:- rdf_register_prefix(bmp, 'http://purl.bioontology.org/net/brunel/p#').
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(void, 'http://rdfs.org/ns/void#').
:- rdf_register_prefix(dc, 'http://purl.org/dc/elements/1.1/').
:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov-o#').
:- prolog_load_context(directory, Dir),  asserta(user:file_search_path(my_home, Dir)).




%% Defining an auxiliary predicate

:- dynamic index_graph/1.
:- dynamic card/2.
:- dynamic log_stream/1.
:- dynamic match/3.
:- dynamic matched/1.
:- dynamic registeredEvent/4.
:- rdf_meta aux(r, r, r).
:- rdf_meta createFreshObject(r, r).
:- rdf_meta createIdObject(-, r, -).
:- rdf_meta sparqlTerm(r, -).


%% RDFS materialization:

%%%%%%%%%%%%%%%%% Certainty into table module + test run model for a single file


%% seetings

%:- asserta(user:file_search_path(index_data, '//acfs4/cssf/csstssk/desktop/biomaterialy/evaluation/machine_with_ihmc_corrected/corrected_cards')).
%:- asserta(user:file_search_path(index_data, 'C:/Users/Szymon/Google Drive/Prolog')).
%:- asserta(user:file_search_path(index_data, 'C:/Users/csstssk/Google Drive/Prolog')).
:- asserta(user:file_search_path(index_data, 'C:/Users/Administrators/Desktop/Assembly')).

%sparql_setup('52.26.26.74', 80, '/sparql-auth', authorization(digest('dba', 'Simco1!'))).

sparql_setup('127.0.0.1', 80, '/sparql-auth', authorization(digest('dba', 'Simco1!'))).

paf_graph('http://purl.bioontology.org/net/brunel/paf').
event_graph('http://purl.bioontology.org/net/brunel/bm/event_data').
submitter_graph('http://purl.bioontology.org/net/brunel/bm/submitter_data').
source_graph('http://purl.bioontology.org/net/brunel/bm/source_data').
journal_graph('http://purl.bioontology.org/net/brunel/bm/journals_2015').

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

translate2rdf([FileName|Rest]) :-
    translateCard(FileName),!,
    translate2rdf(Rest).

translateCard(FileName) :- 
    indexCard(Index, FileName),
    file_base_name(FileName, Base), 
    string_concat(BaseName, '.json', Base),
    log_write(BaseName),
    print_count100,
    cardElement(Index, ''), !,
    findall(_, unexpected_field, _), !,
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
    log_write('\nCreated: \t'), log_writeln(StatLabel).
   % findall(_, probabilityRecord(Statement, GroundingProbList), _).
    
    %....checkSite(Event)


createIndexCardGraph(GraphURI) :-
    index_graph(GraphURI) -> true;     
            (date(date(Y, M, D)),
            atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/index_card_graph_', Y, M, D], GraphURI),
            addMetaData(indexGraph, GraphURI),
            asserta(index_graph(GraphURI))).
    
addMetaData(indexGraph, Graph) :-
    date(date(Y, M, D)), 
    atomic_list_concat([Y, M, D], '-', Date),
    TargetTriples = [
        [Graph, rdf:'type', void:'Dataset'],
        [Graph, dc:'creator', literal('Big Mechanism')],
        [Graph, dc:'title', literal('Textual evidence extracted from literature.')],
        [Graph, dc:'description', literal('This dataset contains RDF data about a single statement extracted automatically from scientific literature and reported in a single index card submitted to the system.')],
        [Graph, dc:'date', literal(Date)]
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
         TripleA =[not('?x', bmpaf:'hasParticipantA', '?y')],
         LabelA='-')),
    (retriveKeyValue(participantBtype, Path) -> 
        (atomic_list_concat([Path, '/participant_b'], PartBpath),
        createIdObject(ParticipantB, bmpaf:'Participant', PartBpath, GroundingProbListB), 
        TripleB = [['?x', bmpaf:'hasParticipantB', ParticipantB]],
        getLabel(ParticipantB, Graph, LabelB)); 
        (GroundingProbListB=[], 
        TripleB =[not('?x', bmpaf:'hasParticipantB', '?z')],
        LabelB='-')),
    BasicTriples = [
        ['?x', rdf:'type', bmpaf:'Event'],
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
        write('Matched exist. entity: \t'), writeln(EntId),
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
    
    


%participantType('protein_family', bmpaf:'ProteinFamily').
%participantType('complex', bmpaf:'Complex').




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



%%%%%%%%%%%%%%%%%%%%%%%
%   Statement metadata creation (inc. probabilities)
%%%%%%%%%%%%%%%%%%%%

probabilityRecord(Statement) :- 
    card('/certainty_level/certainty_level_predefined', CertLevel),
    card('/certainty_level/certainty_level_freeform', CertLevelFree), !,
    createClassedObject('probability', bmp:'ProbabilityRelevantToBiologicalUncertainty', Probability), 
    writeInTargetGraph(Statement, bmp:'hasBiologicalProbability', Probability),
    insertDataValue(Probability, bmp:'hasProbabilityLevel', CertLevel),
    insertDataValue(Probability, bmp:'hasProbabilityLevelDescritpion', CertLevelFree).

probabilityRecord(Statement) :- 
    card('/meta/speculated_information', SpecInf),
    createClassedObject('probability', bmp:'ProbabilityRelevantToTextualUncertainty', Probability), 
    writeInTargetGraph(Statement, bmp:'hasTextualProbability', Probability),
    speculated(SpecInf, Value),
    insertDataValue(Probability, bmp:'hasProbabilityLevel', Value).
    
probabilityRecord(Statement) :- 
    card('/meta/confidence', SpecInf),
    createClassedObject('probability', bmp:'AccuracyOfExtractionFromText', Probability), 
    writeInTargetGraph(Statement, bmp:'hasExtractionAccurracy', Probability),
    insertDataValue(Probability, bmp:'hasProbabilityLevel', SpecInf).
    
speculated(@false, 'certain').
speculated(@true, 'uncertain').

assertProbability :- 
    rdf(X, bmp:'hasSJRscore',  literal(type('http://www.w3.org/2001/XMLSchema#decimal', No))),
    NormalSjr is sqrt(round((No / 40) * 1000) / 1000),
    rdf(Art, bmpaf:'publishedIn', X),
    createClassedObject('probability', bmp:'ProbabilityRelevantToDocumentProvenance', Prob),
    insertDataValue(Prob, bmp:'hasProbabilityLevel', NormalSjr),
    rdf(Stat, bmpaf:'isExtractedFrom', Art),    
    writeInTargetGraph(Stat, bmp:'hasProvenanceProbability', Prob),
    writeInTargetGraph(Art, bmp:'hasProvenanceProbability', Prob).

%%%%%%%%%%%%%%%%%%%%
% generic predicates
%%%%%%%%%%%%%%%%%%%


createFreshObject(TypeUri, Subject) :-
    uuid(U), 
    rdf_current_prefix(_, Expansion),
    atom_concat(Expansion, Local, TypeUri), !,
    downcase_atom(Local, Name),
    atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/', Name, '_', U], Subject).

createIdURI(simple, Id, IdURI) :-
    string_concat('CHEBI:', Number, Id),
    string_concat('http://purl.obolibrary.org/obo/CHEBI_', Number, IdURI).

createIdURI(simple, Id, IdURI) :-
    string_concat('UNIPROT:', Number, Id),
    string_concat('http://purl.uniprot.org/uniprot/', Number, IdURI).

createIdURI(article, Id, IdURI) :-
    string_concat('http://www.ncbi.nlm.nih.gov/pmc/articles/', Id, IdURI).

    
upperCase(Value, ValUpper) :-
    string_upper(Value, ValueUpper),
    atom_string(ValUpper, ValueUpper).


%%%%%%%
% labeling
%%%%%%%%%%%%%%%%


labelingPart :- 
    readInTargetGraph(X, bmpaf:'hasEntityId', Label),
    rdf_literal_value(Label, ProtId),
    upperCase(ProtId, ProtIDupper),
    string_concat('CHEBI:', Number, ProtIDupper),
    string_concat('http://purl.obolibrary.org/obo/CHEBI_', Number, ChebiURI),
    write(ProtId), write('... '),
    atom_string(ChebiURIAtom, ChebiURI),
    rdf(ChebiURIAtom, rdfs:label, Literal),
    rdf_literal_value(Literal, Name),
    writeln(Name),
    insertDataValue(X, bmpaf:'hasEntityName', Name),
    print_count10.

labelingPart :- 
    readInTargetGraph(X, bmpaf:'hasEntityId', Label),
    rdf_literal_value(Label, ProtId),
    upperCase(ProtId, ProtIDupper),
    string_concat('UNIPROT', _, ProtIDupper),
    write(ProtId), write('... '),
    identifierResolution(ProtId, Name),
    writeln(Name),
    insertDataValue(X, bmpaf:'hasEntityName', Name),
    print_count10.

labelingPart :- 
    readInTargetGraph(X, bmpaf:'hasEntityName', Label),
    \+readInTargetGraph(X, rdfs:'label', _),
    writeInTargetGraph(X, rdfs:'label', Label).

labelingPart :- 
    readInTargetGraph(X, bmpaf:'hasEntityText', Label),
    \+readInTargetGraph(X, rdfs:'label', _),
    writeInTargetGraph(X, rdfs:'label', Label).

labelingEvent :- 
    readInTargetGraph(X, rdf:'type', bmpaf:'Event'),
    \+complexPart(X),
    interactionLab(X, ZVal),
    partALab(X, AVal),
    partBLab(X, BVal),
    atomic_list_concat(['(', AVal, ' ', ZVal, ' ', BVal, ')'], Label),
    insertDataValue(X, rdfs:'label', Label).

labelingEvent :- 
    readInTargetGraph(X, rdf:'type', bmpaf:'Event'),
    complexPart(X), 
    interactionLab(X, ZVal),
    partALab(X, AVal),
    partBLab(X, BVal),
    atomic_list_concat(['(', AVal, ' ', ZVal, ' ', BVal, ')'], Label),
    insertDataValue(X, rdfs:'label', Label).

labelingStat :-
    readInTargetGraph(Statement, rdf:'type', bmpaf:'Statement'),
    readInTargetGraph(Statement, bmpaf:'represents', Event),
    readInTargetGraph(Event, rdfs:'label', LabelLiteral),
    readInTargetGraph(Statement, bmpaf:'hasTruthValue', ValueLiteral),
    readInTargetGraph(Statement, bmpaf:'isExtractedFrom', Article),
    readInTargetGraph(Article, rdfs:'label', PMCLiteral),
    rdf_literal_value(ValueLiteral, Value),
    rdf_literal_value(LabelLiteral, Label),
    rdf_literal_value(PMCLiteral, PMC),
    atomic_list_concat([Label, ' is ', Value, ' in ', PMC], StatementLabel),
    insertDataValue(Statement, rdfs:'label', StatementLabel).

labeling :- 
    readInTargetGraph(X, bmpaf:'hasPmcId', Label),
    writeInTargetGraph(X, rdfs:'label', Label).

labeling :- 
    readInTargetGraph(X, bmpaf:'hasIndexCardId', Label),
    writeInTargetGraph(X, rdfs:'label', Label).

labeling :- 
    readInTargetGraph(X, bmpaf:'hasSubmitterId', Label),
    writeInTargetGraph(X, rdfs:'label', Label).
    
labeling :- 
    readInTargetGraph(Probability, rdf:'type', X),
    rdfs_subclass_of(X, bmp:'Probability'),
    readInTargetGraph(Probability, bmp:'hasProbabilityLevel', Level),
    rdf_literal_value(Level, LevelLabel),
    insertDataValue(Probability, rdfs:'label', LevelLabel).
    
%labelingP :-
%    readInTargetGraph(_, bmp:'hasExtractionAccurracy', X),
%    writeInTargetGraph(X, rdf:type, bmp:'AccuracyOfExtractionFromText').

%labelingP :-
%    readInTargetGraph(_, bmp:'hasTextualProbability', X),
%    writeInTargetGraph(X, rdf:type, bmp:'ProbabilityRelevantToTextualUncertainty').

journalLabeling :-
    readInTargetGraph(Journal, rdf:'type', bmpaf:'Journal'),
    readInTargetGraph(Journal, bmpaf:'hasTitle', Title),
    writeInTargetGraph(Journal, rdfs:'label', Title).

interactionLab(X, ZVal) :- 
    mostSpecificType(X, Z, 'http://purl.bioontology.org/net/brunel/paf#Event'),
    \+Z = 'http://purl.bioontology.org/net/brunel/paf#Event',
    rdf(Z, rdfs:'label', ZLabel),
    rdf_literal_value(ZLabel, ZVal), !.
    
interactionLab(_, '-').
    
mostSpecificType(X, Y, Z) :-
    readInTargetGraph(X, rdf:'type', Y), 
    rdfs_subclass_of(Y, Z),
    \+rdf_equal(Y, Z),
    \+mostSpecificType(X, _, Y).

complexPart(X) :- 
    readInTargetGraph(X, bmpaf:'hasParticipantA', A),
    readInTargetGraph(A, rdf:type, bmpaf:'Event').

complexPart(X) :- 
    readInTargetGraph(X, bmpaf:'hasParticipantB', B),
    readInTargetGraph(B, rdf:type, bmpaf:'Event').

    
partALab(X, AVal) :-
    readInTargetGraph(X, bmpaf:'hasParticipantA', A),
    readInTargetGraph(A, rdfs:'label', ALabel),
    rdf_literal_value(ALabel, AVal).    

partALab(X, '-') :-
    \+readInTargetGraph(X, bmpaf:'hasParticipantA', _).
    
partBLab(X, BVal) :-
    readInTargetGraph(X, bmpaf:'hasParticipantB', B),
    readInTargetGraph(B, rdfs:'label', BLabel),
    rdf_literal_value(BLabel, BVal).

partBLab(X, '-'):-
    \+readInTargetGraph(X, bmpaf:'hasParticipantB', _).

checkSite(Event) :-
    card(Key, Value),
    string_concat(_, 'site', Key),
    insertDataValue(Event, bmpaf:'hasSite', Value), !.

checkSite(_).
    
unexpected_field :- 
    card(Key, Value),
    member(X, ['features', 'modifications']),
    string_concat(_, X, Key),
    log_write(Key), 
    log_write(':'), 
    log_writeln(Value).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   RDF graph manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


saveGraph(Graph) :- 
    file_search_path(my_home, Dir),
    atomic_list_concat([Dir, '/', 'output', '.rdf'], OutputPath),
    rdf_save(OutputPath, [graph(Graph), encoding(utf8)]), !.

%pushGraph(RDFgraph) :-
%    findall([X, Y, Z], rdf(X, Y, Z, RDFgraph), GraphPattern),
%    sparqlInsertQuery(GraphPattern, RDFgraph),
%    rdf_retractall(_, _, _, RDFgraph).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPARQL manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


askSparqlQuery(Query, Result) :-
    sparql_setup(Host, Port, Path, Author),
    sparql_query(Query, Result, [host(Host), port(Port), path(Path), status_code(_), Author]).

askHttpSparql(Query, Format, Result, Code) :-
    sparql_setup(Host, Port, Path, Author),
    http_open([ host(Host), port(Port), path(Path), search([query=Query, format=Format])], Result, [status_code(Code), Author]).

sparqlJsonQuery(Query, Tuple) :-
    askHttpSparql(Query, 'application/json', JsonStream, Code), 
    (\+Code = 200 -> (Tuple=[], atomic_list_concat(['Error: ', Code, ' on ', Query], ErrorMessage), writeln(ErrorMessage), true); 
    (json_read(JsonStream, json(Result)),
    member((results=json(X)),Result), 
    member((bindings=Z), X),
    member(json(JsonTup), Z),
    findall(Val, (member(_=json(Bind), JsonTup),
                  member(value=Val, Bind)), Tuple))).

sparqlInsertQuery(Graph, RDFgraph) :-  
    triplePatternGenerator(Graph, TriplePattern), !, 
    atomic_list_concat(['INSERT {GRAPH <', RDFgraph,'> {', TriplePattern, '}}'], Query),
    log_writeln(''), log_writeln(Query),
    sparql_setup(Host, Port, Path, Author),
    http_open([host(Host), port(Port), path(Path), search([query=Query])], _, [Author]).
                  
sparqlSelectQuery(Graph, RDFgraph, Tuple) :-  
    triplePatternGenerator(Graph, TriplePattern), !, 
    atomic_list_concat(['SELECT * WHERE {GRAPH <', RDFgraph,'> {', TriplePattern, '}}'], Query),
    log_writeln(''), log_writeln(Query),
    sparqlJsonQuery(Query, Tuple).

triplePatternGenerator(Graph, TriplePattern) :-
    tripleSetGenerator(Graph, TripleSet),   
    atomic_list_concat(TripleSet, '. ', TriplePattern).    
    
tripleSetGenerator([], []) :- !.

tripleSetGenerator([[X, Y, Z]|Rest], [Triple|RestTriples]) :-
    tripleSetGenerator(Rest, RestTriples),
    sparqlTerm(X, Xterm),
    sparqlTerm(Y, Yterm),
    sparqlTerm(Z, Zterm),
    atomic_list_concat([Xterm, Yterm, Zterm], ' ', Triple).

tripleSetGenerator([not(X, Y, Z)|Rest], [Triple|RestTriples]) :-
    tripleSetGenerator(Rest, RestTriples),
    sparqlTerm(X, Xterm),
    sparqlTerm(Y, Yterm),
    sparqlTerm(Z, Zterm),
    atomic_list_concat(['filter not exists {', Xterm, Yterm, Zterm, '}'], ' ', Triple).

substitute(_, _, [], []) :- !.

substitute(Var, Term, [OldTriple|OldRest], [NewTriple|NewRest]) :-
    \+OldTriple = not(_, _, _),
    (member(Var, OldTriple) -> select(Var, OldTriple, Term, NewTriple); NewTriple=OldTriple),
    substitute(Var, Term, OldRest, NewRest), !.  

substitute(Var, Term, [not(_, _, _)|OldRest], NewRest) :-
    substitute(Var, Term, OldRest, NewRest), !.
    
sparqlTerm(X, X) :-
    atomic(X),
    string_concat('?', _, X), !.

sparqlTerm(literal(X), Term) :-
    makeLiteral(X, Term), !.
    
sparqlTerm(X, Term) :-
    \+X = literal(_),
    rdf_global_id(X, URI),
    atomic_list_concat(['<', URI, '>'], Term), !.

makeLiteral(X, RDFLit) :- 
    atomic_list_concat([Y, M, D], '-', X), 
    atom_number(Y, Yn), atom_number(M, Mn), atom_number(D, Dn), 
    1900 < Yn, Yn < 2099, 0 < Mn, Mn < 13, 0< Dn, Dn <32, !,
    atomic_list_concat(['"', X, '"', '^^xsd:date'], RDFLit).
makeLiteral(X, RDFLit) :- 
    integer(X), !,
    atomic_list_concat(['"', X, '"', '^^xsd:integer'], RDFLit).
makeLiteral(X, RDFLit) :- 
    number(X), 
    \+integer(X), !,
    atomic_list_concat(['"', X, '"', '^^xsd:double'], RDFLit).
makeLiteral(X, RDFLit) :- 
    atomic_list_concat(['"', X, '"'], RDFLit), !.

%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPARQL-based Id resolution
%%%%%%%%%%%%%%%%%%%%%%%%%

sparql_setupy('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> <http://purl.uniprot.org/core/mnemonic> ?x}', 'sparql.uniprot.org', '/sparql/').
%sparql_setupy('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> rdfs:label ?x}', 'sparql.uniprot.org', '/sparql/').
sparql_setupy('CHEBI:', 'SELECT ?x WHERE {<http://purl.obolibrary.org/obo/CHEBI_', '> rdfs:label ?x}', 'chebi.bio2rdf.org', '/sparql').
sparql_setupy('HGNC:', 'SELECT ?x WHERE { <http://bio2rdf.org/hgnc:', '> <http://bio2rdf.org/hgnc_vocabulary:approved-symbol> ?x}', 'hgnc.bio2rdf.org', '/sparql').

identifierResolution(ProtID, Name) :-
    upperCase(ProtID, ProtIDupper),
    sparql_setupy(Prefix, QueryFront, QueryBack, Host, Path),
    string_concat(Prefix, Number, ProtIDupper),
    atomic_list_concat([QueryFront, Number, QueryBack], Query),
    sparql_query(Query, row(Literal), [host(Host), path(Path), status_code(_)]),
    rdf_literal_value(Literal, Name), !.









