:- use_module(library(gensym)).

:- reset_gensym.

:- ['BMsemWebTools.pl'].

%:- rdf_load('datasetNactem20160101.rdf', [graph(graph)]).
%:- rdf_load('journals-scimagojr-xlsx.rdf', [graph(graph2)]).
%:- rdf_load('C:/chebi.owl', [graph(chebi)]).


%% Defining auxiliary predicates

:- dynamic index_graph/1.
:- dynamic card/2.
:- dynamic match/3.
:- dynamic matched/1, journal/1.
:- dynamic registeredEvent/4.
:- rdf_meta aux(r, r, r).
:- rdf_meta createFreshObject(r, r).
%:- rdf_meta participantType(-, r).
%:- rdf_meta interactionType(-, r).
%:- rdf_meta modificationType(-, r).
%:- rdf_meta readerType(-, r).
:- rdf_meta createIdObject(-, r, -).


%% top-level control

run :- 
    openOutputs,
    loadAuxiliaryData, writeln('Loaded data!'),
    run(1), !.

run(N) :-
    log_stream(Stream),
    findall(_, assembleNextFolder(N), _),
    rdf_retractall(_, _, _),
    closeOutputs,
    close(Stream).
    
loadAuxiliaryData :-
    rdf_load('journal_graph_2015.rdf', [graph(journals)]),
    rdf_load('PANDA.OWL', [graph(panda)]),
    csv_read_file('NameTables/uniprot.csv', Rows1), 
    findall(_, (
        member(row(X, Y), Rows1),
        assert(name(X, Y))
        ), _),
    csv_read_file('NameTables/chebi.csv', Rows2), 
    findall(_, (
        member(row(X, Y), Rows2),
        assert(name(X, Y))
        ), _),
    csv_read_file('NameTables/hgnc.csv', Rows3), 
    findall(_, (
        member(row(X, Y), Rows3),
        assert(name(X, Y))
        ), _).

assembleNextFolder(N) :- 
    indexFolderPath(Folder), !,
    nl, write('Collecting index card files from '), write(Folder), writeln('...'),
    filecollector(Folder, FileList), 
    length(FileList, ListLength),
    write(ListLength), writeln(' files collected.'),
    nl, writeln('Assembling index cards...'),
    translate2rdf(FileList, N).

filecollector(Folder, FileList) :-
    file_search_path(index_data, Dir), 
    string_concat(Dir, Folder, Path),
    writeln(Path),
    expand_file_name(Path, FileList), !.
    
translate2rdf(FileList, M) :-
    length(FileList, L),
    L < M.

translate2rdf(FileList, N) :-
    nth1(N, FileList, FileName),
    nl, write(N), write(': Processing index card: \t'), writeln(FileName),
    translateCard(FileName),
    M is N+1,
    translate2rdf(FileList, M).
    
translateCard(FileName) :- 
    retractall(card(_, _)),
    indexCard(Index, FileName),
    file_base_name(FileName, Base), 
    string_concat(BaseName, '.json', Base),
    log_write(BaseName),
    print_count100,
    cardElement(Index, ''), !, readln(_),
    (createRDF(BaseName) -> (log_writeln('\nSuccess!\n\n'), writeln('Success!'));(log_writeln('\nFail!\n\n'), writeln('Fail!'), readln(_))).

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

%from index cards

createRDF(BaseName) :- 
    createIndexCardGraph(GraphURI), 
    createFreshObject(panda:'IndexCard', IndexCard), !,
    createIdObject(Event, panda:'Event', '/interaction', GroundingProbList, EventLabel, _),
    createFreshObject(panda:'Statement', Statement),
    createIdObject(Submitter, panda:'Submitter', '', []),
    createIdObject(Article, panda:'JournalArticle', '', []),
    retriveKeyValue(readingStarted, StartTime),
    retriveKeyValue(readingStopped, StopTime),
    retriveKeyValue(truthValue, TruthValue),
    retriveKeyValue(readerType, ReaderType),
    retriveKeyValue(submitter, SubmitLab),
    retriveKeyValue(pmcId, PMCid),
    atomic_list_concat(['{', EventLabel, '} ', TruthValue, ' in ', PMCid, ' (by ', SubmitLab ,')'], StatLabel),
    BasicTriples = [
        [IndexCard, rdf:'type', panda:'IndexCard'],
        [IndexCard, panda:'hasStartTime', literal(StartTime)],
        [IndexCard, panda:'hasStopTime', literal(StopTime)],
        [IndexCard, panda:'hasIndexCardId', literal(BaseName)],
        [Statement, panda:'hasAnnotator', ReaderType],
        [IndexCard, panda:'containsStatement', Statement],
        [Statement, rdf:'type', panda:'Statement'],
        [Statement, panda:'containedIn', IndexCard],
        [Statement, rdf:'type', prov:'Entity'],
        [Statement, rdfs:'label', literal(StatLabel)],
        [Statement, panda:'hasTruthValue', literal(TruthValue)],
        [Statement, panda:'represents', Event],
        [Statement, panda:'hasSubmitter', Submitter],
        [Event, panda:'isRepresentedBy', Statement],
        [Statement, panda:'isExtractedFrom', Article]
        ],
    findall([Statement, panda:'hasTextualEvidence', literal(EvidenceString)], (card('/evidence', Evidence), clearString(Evidence, EvidenceString)), EvidenceTriples),
    union(BasicTriples, EvidenceTriples, TargetTriples),
    write('Created new statement: \t'), writeln(StatLabel),
    log_write('\nCreated: \t'), log_writeln(StatLabel),
    findall(Triple, (uncertaintyInput(Statement, ProbTriples), member(Triple, ProbTriples)), FirstProbTriples),
    findall(Triple, (uncertaintyInput(Statement, GroundingProbList, ProbTriples), member(Triple, ProbTriples)), GroundProbTriples),
    union(FirstProbTriples, GroundProbTriples, TargetProbTriples),
    union(TargetTriples, TargetProbTriples, TargetAllTriples),
    pushTriples(TargetAllTriples, GraphURI).


createIndexCardGraph(Graph) :-
    index_graph(Graph) -> true;     
            (date(date(Y, M, D)),
            atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/index_card_graph'], Graph), 
            atomic_list_concat([Y, M, D], '-', Date),
    	    atomic_list_concat([Graph, '.rdf'], GraphDump),
	    atomic_list_concat([Graph, '.graph'], GraphBrowse),
            TargetTriples = [
                [Graph, rdf:'type', void:'Dataset'],
                [Graph, dc:'creator', literal('Big Mechanism')],
                [Graph, dc:'title', literal('Textual evidence extracted from literature')],
                [Graph, dc:'description', literal('Data about statements extracted automatically from scientific literature and reported in a collection of index card submitted to the system.')],
                [Graph, dc:'date', literal(Date)],
		[Graph, void:'dataDump', GraphDump],
                [Graph, void:'dataBrowse', GraphBrowse]
                ],
            pushTriples(TargetTriples, Graph),
            asserta(index_graph(Graph))).

    

%%%%%%%%%%%%%%%%%%%%%%%
%   Entity creation
%%%%%%%%%%%%%%%%%%%%%%%

createIdObject(Event, panda:'Event', Path, GroundingProbList, EventLabel, EventURIstr) :-
    retriveKeyValue(interactionType, Path, EvTypURI),
    getParticipantInfo(a, EvTypURI, Path, TripleA, LabelA, GroundingProbListA, URIstrA),
    getParticipantInfo(b, EvTypURI, Path, TripleB, LabelB, GroundingProbListB, URIstrB),
    append([GroundingProbListA, GroundingProbListB], GroundingProbList),
    BasicTriples = [
        ['?x', rdf:'type', EvTypURI]],
    union(TripleA, TripleB, PartTriples),
    \+PartTriples = [],
    union(BasicTriples, PartTriples, EventTriples),
    event_graph(Graph),
    rdf_global_id(EvTypURI, EvTypURIGlob),
    rdf(EvTypURIGlob, rdfs:'label', EventTypeLabelLiteral),
    rdf_literal_value(EventTypeLabelLiteral, EventTypeLabel),
    atomic_list_concat(['(', LabelA, ') ', EventTypeLabel, ' (', LabelB, ')'], EventLabel),
    (\+member('', [URIstrA,URIstrB]) ->
         (atomic_list_concat(['(', URIstrA, ') ', EventTypeLabel, ' (', URIstrB, ')'], EventURIstr),
         label2URI(EventURIstr, Event));
         (EventURIstr='',
         createFreshObject(panda:'Event', Event))),
    log_write('\nCreated '), log_writeln(EventLabel),
    write('Created new event: \t'), writeln(EventLabel),
    LabelTriple=[[Event, rdfs:'label', literal(EventLabel)]],
    substitute('?x', Event, EventTriples, EventTriplesSub),
    union(LabelTriple, EventTriplesSub, AllEventTriples),
    pushTriples(AllEventTriples, Graph), !.

createIdObject(Modification, panda:'ProteinModification', Path, GroundingProbListB, EventLabel, EventURIstr) :-
    retriveKeyValue(modificationType, Path, ModType),
    atomic_list_concat([Path, '/participant_b'], PartBpath),
    createIdObject(ParticipantB, panda:'Participant', PartBpath, GroundingProbListB, LabelB, PartURIstr), 
    rdf_global_id(ModType, ModTypeGlob),
    rdf(ModTypeGlob, rdfs:'label', ModTypeLabelLiteral), 
    rdf_literal_value(ModTypeLabelLiteral, ModTypeLabel),
    atomic_list_concat([ModTypeLabel, ' of (', LabelB, ')'], EventLabel),
    event_graph(Graph),
    (\+PartURIstr='' ->
        (atomic_list_concat([ModTypeLabel, ' (', PartURIstr, ')'], EventURIstr),
        label2URI(EventURIstr, Modification));
        (EventURIstr='',
        createFreshObject(panda:'ProteinModification', Modification))),
    log_write('\nCreated '), log_writeln(EventLabel),
    write('Created new event: \t'), writeln(EventLabel),
    ModificationTriples = [
                 [Modification, panda:'hasParticipantB', ParticipantB],
                 [Modification, rdf:'type', ModType],
                 [Modification, rdfs:'label', literal(EventLabel)]
                 ],
     pushTriples(ModificationTriples, Graph).    
        
createIdObject(GeneExpression, panda:'GeneExpression', Path, GroundingProbListB, EventLabel, EventURIstr) :-
    atomic_list_concat([Path, '/participant_b'], PartBpath),
    createIdObject(ParticipantB, panda:'Participant', PartBpath, GroundingProbListB, LabelB, PartURIstr), 
    atomic_list_concat(['gene expression of (', LabelB, ')'], EventLabel),  
    event_graph(Graph),
    (\+PartURIstr='' ->
        (atomic_list_concat(['gene expression (', PartURIstr, ')'], EventURIstr),
        label2URI(EventURIstr, GeneExpression));
        (EventURIstr='',
        createFreshObject(panda:'GeneExpression', GeneExpression))),
        log_write('\nCreated '), log_writeln(EventLabel),
        write('Created new event: \t'), writeln(EventLabel),
        ExpressionTriples = [
                    [GeneExpression, panda:'hasParticipantB', ParticipantB],
                    [GeneExpression, rdf:'type',  panda:'GeneExpression'],
                    [GeneExpression, rdfs:'label', literal(EventLabel)]
                    ],
        pushTriples(ExpressionTriples, Graph).    
    
createIdObject(Participant, panda:'Participant', Path, GroundingProbList, Label, URIstr) :-
    retriveKeyValue(entityType, Path, EntType),
    createIdObject(Participant, EntType, Path, GroundingProbList, Label, URIstr), !.   
    
createIdObject(SimpleURI, SimpleType, Path, [GroundingProb], Label, URIstr) :-
    member(SimpleType, [panda:'Protein', panda:'Chemical', panda:'Gene']), !,
    retriveKeyValue(entityId, Path, EntId),
    retriveKeyValue(entityText, Path, EntText),
    retriveKeyValue(grounding, Path, GroundingProb),
    fixEntId(EntId, FixedEntId), 
    event_graph(Graph),
    getObjectTriples(SimpleURI, FixedEntId, EntText, SimpleType, Triples, Label, URIstr),
    pushTriples(Triples, Graph), !.
    
createIdObject(Submitter, panda:'Submitter', '', []) :-
    retriveKeyValue(submitter, SubmitterName),
    submitter_graph(Graph),
    createIdURI(submitter, SubmitterName, Submitter),
    TargetTriples = [
        [Submitter, panda:'hasSubmitterId', literal(SubmitterName)],
        [Submitter, rdf:'type', panda:'Submitter'],
        [Submitter, rdfs:'label', literal(SubmitterName)]
        ],
    pushTriples(TargetTriples, Graph).

createIdObject(Article, panda:'JournalArticle', '', []) :-
    retriveKeyValue(pmcId, PmcId),
    source_graph(Graph),
    createIdURI(article, PmcId, Article),
    pubmedJournalTitleYear(PmcId, ISSN, Title, Year),
    createIdObject(Journal, panda:'Journal', ISSN, []),
    asserta(journal(Journal)), 
    TargetTriples = [
         [Article, panda:'hasPMCId', literal(PmcId)],
         [Article, panda:'hasTitle', literal(Title)],
         [Article, rdfs:'label', literal(Title)],
         [Article, rdf:'type', panda:'JournalArticle'],
         [Article, panda:'publishedIn', Journal],
         [Article, panda:'hasPublicationYear', literal(Year)]
     ],
    pushTriples(TargetTriples, Graph).

createIdObject(Journal, panda:'Journal', ISSN, []) :-
    (rdf(X, panda:'hasISSN', literal(ISSN)) -> 
        X=Journal; 
        (createFreshObject(panda:'Journal', Journal),
        TargetTriples = [
        [Journal, rdf:'type', panda:'Journal'],
        [Journal, panda:'hasISSN', literal(ISSN)]
        ],
        journal_graph(Graph),
        pushTriples(TargetTriples, Graph))).
        
pubmedJournalTitleYear(PmcId, ISSN, Title, Year) :- 
    atomic_list_concat(['http://www.ncbi.nlm.nih.gov/pmc/utils/ctxp/?ids=', PmcId, '&report=citeproc&format=json'], Query),
    ((http_open(Query, In, [connection('Keep-alive'), status_code(200)]), set_stream(In, encoding(utf8))) ->
    (json_read(In, json(X)), 
    member('ISSN'=ISSNproper, X), !,
    atomic_list_concat(List, '-', ISSNproper),
    atomic_list_concat(List, ISSN),
    member('title'=TitleString, X), !,
    clearString(TitleString, Title),
    member(issued=json(['date-parts'=[[Year | _]]]), X), !); 
    (write("Problem with http: "), writeln(PmcId),
    sleep(5),
    pubmedJournalTitleYear(PmcId, ISSN, Title, Year))).

getParticipantInfo(a, _, Path, TripleA, LabelA, GroundingProbListA, URIstrA) :-
    retriveKeyValue(participantAtype, Path),
    atomic_list_concat([Path, '/participant_a'], PartApath),
    createIdObject(ParticipantA, panda:'Participant', PartApath, GroundingProbListA, LabelA, URIstrA), 
    TripleA = [['?x', panda:'hasParticipantA', ParticipantA]], !.

getParticipantInfo(a, _, _, [], '-', [], '-').

getParticipantInfo(b, EvTypURI, Path, TripleB, LabelB, GroundingProbListB, URIstrB) :-
    member(EvTypURI, [panda:'AddsModification', panda:'InhibitsModification']),
    retriveKeyValue(participantBtype, Path),
    createIdObject(ParticipantB, panda:'ProteinModification', Path, GroundingProbListB, LabelB, URIstrB), 
    TripleB = [['?x', panda:'hasParticipantB', ParticipantB]], !.
    
%getParticipantInfo(b, panda:'GeneExpression', Path, TripleB, LabelB, GroundingProbListB, URIstrB) :-
%    retriveKeyValue(participantBtype, Path),
%    createIdObject(ParticipantB, panda:'GeneExpression', Path, GroundingProbListB, LabelB, URIstrB), 
%    TripleB = [['?x', panda:'hasParticipantB', ParticipantB]], !.
    
getParticipantInfo(b, _, Path, TripleB, LabelB, GroundingProbListB, URIstrB) :-
    retriveKeyValue(participantBtype, Path),
    atomic_list_concat([Path, '/participant_b'], PartBpath),
    createIdObject(ParticipantB, panda:'Participant', PartBpath, GroundingProbListB, LabelB, URIstrB), 
    TripleB = [['?x', panda:'hasParticipantB', ParticipantB]], !.
    
getParticipantInfo(b, _, _, [], '-', [], '-').

getObjectTriples(SimpleURI, EntId, EntText, SimpleType, Triples, Name, EntId) :-
    \+EntId = 'GENERIC',  
    identifierResolution(EntId, Name),
    createIdURI(simple, EntId, SimpleURI),
    log_write('\nCreated '), log_writeln(EntId),
    write('Created new entity: \t'), writeln(EntId), 
    Triples = [ [SimpleURI, panda:'hasEntityText', literal(EntText)],
                [SimpleURI, rdf:'type', SimpleType],
                [SimpleURI, panda:'hasEntityId', literal(EntId)],
                [SimpleURI, panda:'hasEntityName', literal(Name)],
                [SimpleURI, rdfs:'label', literal(Name)]], !.   
                
getObjectTriples(SimpleURI, EntId, EntText, SimpleType, Triples, EntId, EntId) :-
    \+EntId = 'GENERIC', 
    createIdURI(simple, EntId, SimpleURI),
    log_write('\nCreated '), log_writeln(EntId),
    write('Created new entity: \t'), writeln(EntId),
    Triples = [ [SimpleURI, panda:'hasEntityText', literal(EntText)],
                [SimpleURI, rdf:'type', SimpleType],
                [SimpleURI, panda:'hasEntityId', literal(EntId)],
                [SimpleURI, rdfs:'label', literal(EntId)]], !.            

getObjectTriples(SimpleURI, 'GENERIC', EntText, SimpleType, Triples, Text, '') :-
    createIdURI(simple, 'GENERIC', SimpleURI),
    atomic_list_concat(['no ID', ' [', EntText, ']'], Text),
    log_writeln('\nCreated generic entity'),
    write('Created new entity: \t'), writeln(Text),
    Triples = [ [SimpleURI, panda:'hasEntityText', literal(EntText)],
                [SimpleURI, rdf:'type', SimpleType],
                [SimpleURI, rdfs:'label', literal(Text)]], !. 
    

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
    card('/reading_started', StartTime), !.
    
retriveKeyValue(readingStarted, '2000-1-1').
    
retriveKeyValue(readingStopped, StopTime) :-
    card('/reading_complete', StopTime), !.

retriveKeyValue(readingStopped, '2000-1-1').

retriveKeyValue(submitter, Submitter) :-
    card('/submitter', SubmitterString),
    upperCase(SubmitterString, SubmitterUpper),
    normalize_space(atom(Submitter), SubmitterUpper).
    
retriveKeyValue(readerType, ReaderTypeURI)  :-
    (card('/reader_type', ReaderType) -> 
        readerType(ReaderType, ReaderTypeURI);
        ReaderTypeURI = panda:'Annotator').

retriveKeyValue(truthValue, TruthValue) :-
    card('/interaction/negative_information', NegInf), 
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
    card('/meta/uncertain', SpecInf),
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
    card(Key, EntTextDirty),
    clearString(EntTextDirty, EntText).
    
retriveKeyValue(modificationType, Path, ModType) :-
    atomic_list_concat([Path, '/modification_type'], Key),
    card(Key, Value),
    modificationType(Value, ModType).

retriveKeyValue(grounding, Path, Value) :-
     atomic_list_concat([Path, '/grounding_score'], Key),
     card(Key, Value), !.

retriveKeyValue(grounding, Path, 0) :-
    retriveKeyValue(entityId, Path, 'GENERIC'); 
    \+retriveKeyValue(entityId, Path, _), !.

retriveKeyValue(grounding, _, 1).    

participantType('protein', panda:'Protein').
participantType('chemical', panda:'Chemical').
participantType('protein_family', panda:'ProteinFamily').
participantType('gene', panda:'Gene').
participantType('complex', panda:'Complex').
participantType('event', panda:'Event').
    
interactionType('binds', panda:'Binding').
interactionType('increases', panda:'PositiveRegulation').
interactionType('decreases', panda:'NegativeRegulation').
interactionType('positive_regulation', panda:'PositiveRegulation').
interactionType('negative_regulation', panda:'NegativeRegulation').
interactionType('regulation', panda:'Regulation').
interactionType('translocates', panda:'Translocation').
interactionType('increases_activity', panda:'IncreasesActivity').
interactionType('decreases_activity', panda:'DecreasesActivity').
interactionType('gene_expression', panda:'GeneExpression').
interactionType('adds_modification', panda:'AddsModification').
interactionType('inhibits_modification', panda:'InhibitsModification').
interactionType('removes_modification', panda:'InhibitsModification').

modificationType('phosphorylation', panda:'Phosphorylation').
modificationType('acetylation', panda:'Acetylation').
modificationType('farnesylation', panda:'Farnesylation').
modificationType('glycosylation', panda:'Glycosylation').
modificationType('hydroxylation', panda:'Hydroxylation').
modificationType('methylation', panda:'Methylation').
modificationType('ribosylation', panda:'Ribosylation').
modificationType('sumoylation', panda:'Sumoylation').
modificationType('ubiquitination', panda:'Ubiquitination').
modificationType('protein_modification', panda:'ProteinModification').

readerType('human', panda:'Human'). 
readerType('machine', panda:'Computer').
readerType(ReaderType, panda:'HumanComputer') :-
     member(ReaderType, ['human_machine', 'human-machine']).

negation(@false, true).
negation(@true, false).

speculated(@false, 'certain').
speculated(@true, 'uncertain').

%%%%%%%%%%%%%%%%%%%%%%%
%   Input probabilities recording
%%%%%%%%%%%%%%%%%%%%

uncertaintyInput(Statement, TargetTriples) :- 
    retriveKeyValue(textualUncertainty, Value),
    createFreshObject(uno:'TextualUncertainty', Subject),
    TargetTriples = [
        [Statement, uno:'hasTextualUncertainty', Subject],
        [Subject, rdf:'type', uno:'TextualUncertainty'],
        [Subject, uno:'hasUncertaintyLevel', literal(Value)],
        [Subject, rdfs:'label', literal(Value)]
        ].

uncertaintyInput(Statement, TargetTriples) :- 
    retriveKeyValue(extractionAccurracy, Accurracy),
    createFreshObject(uno:'AccuracyOfExtractionFromText', Subject),
    TargetTriples = [
        [Statement, uno:'hasExtractionAccurracy', Subject],
        [Subject, rdf:'type', uno:'AccuracyOfExtractionFromText'],
        [Subject, uno:'hasUncertaintyLevel', literal(Accurracy)],
        [Subject, rdfs:'label', literal(Accurracy)]
        ].


uncertaintyInput(Statement, TargetTriples) :- 
    journal(Journal), 
    rdf(Journal, panda:'hasSJRscore', ScoreLit),
    rdf_literal_value(ScoreLit, Score),
    %QueryTriples = [
    %    [Journal, panda:'hasSJRscore', '?s']
    %    ],
    %sparqlSelectQueryGlobal(QueryTriples, '?s', [Score]),
    retractall(journal(_)),
    atom_number(Score, ScoreNumber), 
    Prob is (round(sqrt(sqrt(ScoreNumber / 33)) * 1000) / 1000),
    createFreshObject(uno:'UncertaintyRelevantToDocumentProvenance', Subject),
    TargetTriples = [
        [Statement, uno:'hasProvenanceUncertainty', Subject],
        [Subject, rdf:'type', uno:'UncertaintyRelevantToDocumentProvenance'],
        [Subject, uno:'hasUncertaintyLevel', literal(Prob)],
        [Subject, rdfs:'label', literal(Prob)]
        ], !.

uncertaintyInput(Statement, GroundingProbList, TargetTriples) :-
    listAverage(GroundingProbList, Prob),
    createFreshObject(uno:'UncertaintyRelevantToEntityGrounding', Subject),
    TargetTriples = [
        [Statement, uno:'hasGroundingUncertainty', Subject],
        [Subject, rdf:'type', uno:'UncertaintyRelevantToEntityGrounding'],
        [Subject, uno:'hasUncertaintyLevel', literal(Prob)],
        [Subject, rdfs:'label', literal(Prob)]
        ].

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
    
listAverage(List, Average) :-
    sum_list(List, Sum),
    length(List, N),
    Average is round((Sum / N) * 100) / 100.
    
clearString(DirtyString, CleanString) :-
    split_string(DirtyString, "\n\200\\223\\224\\210\\222\", "", StringList),
	subtract(StringList, [""], StringList2),
    atomics_to_string(StringList2, CleanString).
    
%%%%%%%%%%%%%%%%%%%%%
% URI generation
%%%%%%%%%%%%%%%%%%%%%

createIdURI(simple, Id, IdURI) :-
    string_concat('CHEBI:', Number, Id),
    string_concat('http://purl.obolibrary.org/obo/CHEBI_', Number, IdURI).

createIdURI(simple, Id, IdURI) :-
    string_concat('UNIPROT:', Number, Id),
    string_concat('http://purl.uniprot.org/uniprot/', Number, IdURI).

createIdURI(simple, Id, IdURI) :-
    string_concat('HGNC:', Number, Id),
    string_concat('http://bio2rdf.org/hgnc:', Number, IdURI).
    
createIdURI(simple, 'GENERIC', URI) :-
    createFreshObject(panda:'Chemical', URI).

createIdURI(article, Id, IdURI) :-
    string_concat('http://www.ncbi.nlm.nih.gov/pmc/articles/', Id, IdURI).
    
createIdURI(submitter, SubmitterName, SubmitterURI) :-
    atom_string(SubmitAtom, SubmitterName),
    string_concat('http://purl.bioontology.org/net/brunel/bm/submitter_', SubmitAtom, SubmitterURI).



%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPARQL-based Id resolution
%%%%%%%%%%%%%%%%%%%%%%%%%

sparql_params('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> <http://purl.uniprot.org/core/mnemonic> ?x}', 'sparql.uniprot.org', '/sparql/', 80).
%sparql_params('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> rdfs:label ?x}', 'sparql.uniprot.org', '/sparql/', 80).
%sparql_params('CHEBI:', 'SELECT ?x WHERE {<http://purl.obolibrary.org/obo/CHEBI_', '> rdfs:label ?x}', 'chebi.bio2rdf.org', '/sparql', 80).
sparql_params('CHEBI:', 'SELECT ?x WHERE {<http://purl.obolibrary.org/obo/CHEBI_', '> rdfs:label ?x}', '127.0.0.1', '/sparql', 8890).
sparql_params('HGNC:', 'SELECT ?x WHERE { <http://bio2rdf.org/hgnc:', '> <http://bio2rdf.org/hgnc_vocabulary:approved-symbol> ?x}', 'hgnc.bio2rdf.org', '/sparql', 80).

identifierResolution('GENERIC', 'generic') :-  !.

identifierResolution(Id, Name) :- name(Id, Name), !.
    
identifierResolution(Id, Id) :- writeln(['not enough data', Id]).


%identifierResolution(ProtID, Name) :-
%    upperCase(ProtID, ProtIDupper),
%    sparql_params(Prefix, QueryFront, QueryBack, Host, Path, Port),
%    string_concat(Prefix, Number, ProtIDupper),
%    atomic_list_concat([QueryFront, Number, QueryBack], Query),
%    sparql_query(Query, row(Literal), [host(Host), port(Port), path(Path), connection('Keep-alive'), status_code(200)]),
%    rdf_literal_value(Literal, Name), !.



%%%%%%%%%%%%
% different submitters submit UNIPROT Ids in different forms: UNIPROT:Q13480 or UNIPROT:GAB1_HUMAN
% the following predicates makes the representation uniform following the format: UNIPROT:Q13480
%%%%%%%%%%%%

sparql_uniprot_fix_params('SELECT (STRAFTER(str(?x), "http://purl.uniprot.org/uniprot/") as ?n) WHERE {?x <http://purl.uniprot.org/core/mnemonic> "', '"}', 'sparql.uniprot.org', '/sparql/', 80).

fixEntId(ProtId, AltId) :- name(AltId, ProtId), !.

%fixEntId(ProtID, AltId) :-
%    upperCase(ProtID, ProtIDupper),
%    string_concat('UNIPROT:', Id, ProtIDupper),
%    string_codes(Id, Codes), 
%    member(95, Codes),
%    sparql_uniprot_fix_params(QueryFront, QueryBack, Host, Path, Port),
%    atomic_list_concat([QueryFront, Id, QueryBack], Query),
%    sparql_query(Query, row(Literal), [host(Host), port(Port), path(Path), connection('Keep-alive'), status_code(_)]),
%    rdf_literal_value(Literal, BasicId), 
%    string_concat('UNIPROT:', BasicId, AltId), !.
    
fixEntId(ProtID, ProtID).
