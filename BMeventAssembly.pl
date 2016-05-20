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
:- use_module(library(gensym)).

:- rdf_reset_db.
:- reset_gensym.

:- rdf_load('PAF.owl', [graph(pafOnto)]).
:- rdf_load('P.owl', [graph(pOnto)]).
:- rdf_load('datasetNactem20160101.rdf', [graph(graph)]).
:- rdf_load('journals-scimagojr-xlsx.rdf', [graph(graph2)]).
:- rdf_load('C:/chebi.owl', [graph(chebi)]).

:- rdf_register_prefix(nov, 'http://localhost:3333/').
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
:- asserta(user:file_search_path(index_data, '//acfs4/cssf/csstssk/desktop/biomaterialy/evaluation/machine_with_ihmc_corrected/corrected_cards')).
:- asserta(user:file_search_path(index_data2, 'C:/Users/Szymon/Google Drive/Prolog')).
:- asserta(user:file_search_path(index_data3, 'C:/Users/csstssk/Google Drive/Prolog')).



%% Defining an auxiliary predicate

:- dynamic aux/3.
:- dynamic card/2.
:- dynamic log_stream/1.
:- dynamic match/3.
:- dynamic matched/1.
:- dynamic registeredEvent/4.
:- rdf_meta aux(r, r, r).


%% RDFS materialization:

%%%%%%%%%%%%%%%%% Certainty into table module + test run model for a single file


%% top-level control

targetGraph(graph). 



filecollector(Folder, FileList) :-
    %file_search_path(index_data2, Dir), 
    file_search_path(index_data3, Dir), 
    string_concat(Dir, Folder, Path),
    expand_file_name(Path, FileList), !.

assembleNextFolder :- 
    member(Folder, [
        '/IndexNewNactem/*.*'
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



index2rdf :-
    file_search_path(my_home, Dir2), !,
    string_concat(Dir2, '/log_indexCardAssembly.txt', LogFile),
    open(LogFile, write, Stream),
    asserta(log_stream(Stream)), !,
    findall(_, assembleNextFolder, _),
    targetGraph(Graph),
    nl, writeln('Saving the raw data graph...'),
    saveGraph(Graph),
    processData,
    close(Stream).
    
processData :-
    targetGraph(Graph),
    addMetaData(Graph),
%    findall(_, labelingP, _),
    findall([X, Y], rdf(X, bmpaf:'hasEntityId', Y), L), 
    length(L, Length),
    nl, write('Resolving entity identifiers and labels of '), write(Length), writeln(' participant-identifier pairs...'),
    reset_gensym,
    findall(_, labelingPart, _),
    nl, writeln('Retrieving publication metadata...'),
    findall(_, articlePublicationInfo, _),
    findall(_, journalLabeling, _),
    nl, writeln('Saving the graph...'),
    saveGraph(Graph),
    run_journal, !,
    findall(_, labeling, _),
    findall(_, labelingEvent, _),
    findall(_, labelingEvent, _),
    findall(_, labelingStat, _),
    saveGraph(Graph).

translate2rdf([]).

translate2rdf([FileName|Rest]) :-
    translateCard(FileName),
    translate2rdf(Rest).

translateCard(FileName) :- 
    indexCard(Index, FileName),
    file_base_name(FileName, BaseName), 
    log_write(BaseName),
    print_count100,
    cardElement(Index, ''), !,
    findall(_, unexpected_field, _), !,
    findall(_, mapCard(BaseName), _),
    retractall(card(_, _)),
    log_writeln(' ...Done!').

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

%% json index card reader

indexCard(Term, FileName) :- 
    open(FileName, read, IndexCard), 
    json_read(IndexCard, TopTerm),
    TopTerm = json(Term),
    close(IndexCard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% index card content extractor
%  syntax: cardElement(JsonTerm, Key, Value)



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

mapCard(BaseName) :- 
    !,
    createClassedObject('indexCard', bmpaf:'IndexCard', IndexCard),
    insertDataValue(IndexCard, bmpaf:'hasIndexCardId', BaseName),
    createClassedObject('statement', bmpaf:'Statement', Statement),
    writeInTargetGraph(IndexCard, bmpaf:'containsStatement', Statement),
    writeInTargetGraph(Statement, bmpaf:'containedIn', IndexCard),
    card('/pmc_id', PmcId),
    createClassedIdObject('article', bmpaf:'JournalArticle', Article, bmpaf:'hasPmcId', PmcId),
    writeInTargetGraph(Statement, bmpaf:'isExtractedFrom', Article),
    card('/reading_started', StartTime),
    card('/reading_complete', StopTime),
    insertDataValue(IndexCard, bmpaf:'hasStartTime', StartTime),
    insertDataValue(IndexCard, bmpaf:'hasStopTime', StopTime),
    card('/submitter', SubmitterName),
    createClassedIdObject('submitter', bmpaf:'Submitter', Submitter, bmpaf:'hasSubmitterId', SubmitterName),
    writeInTargetGraph(IndexCard, bmpaf:'hasSubmitter', Submitter),
    createFreshObject('annotator', Reader),
    writeInTargetGraph(IndexCard, bmpaf:'hasAnnotator', Reader),
    card('/reader_type', ReaderType),
    assertReader(Reader, ReaderType), 
    findall(_, evidence(Statement), _),
    findall(_, probabilityRecord(Statement), _),
    card('/extracted_information/negative_information', NegInf),
    negation(NegInf, Inf),
    insertDataValue(Statement, bmpaf:'hasTruthValue', Inf),
    createEvent(Event),
    writeInTargetGraph(Event, bmpaf:'isRepresentedBy', Statement),
    checkSite(Event),
    writeInTargetGraph(Statement, bmpaf:'represents', Event).


%%%%%%%%%%%%%%%%%%%%%%%
%   event creation
%%%%%%%%%%%%%%%%%%%%%%%


createEvent(Event) :-
    findall([Subject, PredURI], participant(Subject, PredURI), Participants),
    findall(IntClass, interaction(IntClass), Interaction),
    findall(ModClass, modification(ModClass), Modification),
    generateEvent(Event, Participants, Interaction, Modification).

generateEvent(Event, Participants, Interaction, Modification) :-
    identifyEvent(Event, Participants, Interaction, Modification), !.

generateEvent(Event, Participants, Interaction, Modification) :-
    createClassedObject('event', bmpaf:'Event', Event),
    createParticipants(Event, Participants),
    createInteraction(Event, Interaction),
    createModification(Event, Modification).

createParticipants(_, []).

createParticipants(Event, [[Subject, PredURI]|Rest]) :-
    writeInTargetGraph(Subject, rdf:'type', bmpaf:'Participant'),
    writeInTargetGraph(Event, PredURI, Subject),
    createParticipants(Event, Rest).    

createInteraction(_, []).

createInteraction(Event, [IntClass]) :-
    writeInTargetGraph(Event, rdf:'type', IntClass).
    
createModification(_, []).

createModification(Event, [ModClass|Rest]) :- 
    createClassedObject('modification', ModClass, ModEntity),
    writeInTargetGraph(Event, bmpaf:'hasModificationFeature', ModEntity),
    createModification(Event, Rest).
    
identifyEvent(Event, Participants, Interaction, Modification) :-
    registeredEvent(Event, EvParticipants, EvInteraction, EvModification),
    subset(Participants, EvParticipants),
    subset(Interaction, EvInteraction),
    subset(Modification, EvModification),
    subset(EvParticipants, Participants),
    subset(EvInteraction, Interaction),
    subset(EvModification, Modification).
    
identifyEvent(Event, Participants, Interaction, Modification) :-
    readInTargetGraph(Event, rdf:'type', bmpaf:'Event'),
    \+registeredEvent(Event, _, _, _),
    findall([Subject, PredURI], queryParticipant(Event, Subject, PredURI), EvParticipants),
    findall(IntClass, queryInteraction(Event, IntClass), EvInteraction),
    findall(ModClass, queryModification(Event, ModClass), EvModification),
    assert(registeredEvent(Event, EvParticipants, EvInteraction, EvModification)),
    subset(Participants, EvParticipants),
    subset(Interaction, EvInteraction),
    subset(Modification, EvModification),
    subset(EvParticipants, Participants),
    subset(EvInteraction, Interaction),
    subset(EvModification, Modification), !.

queryParticipant(Event, Subject, PredURI) :-
    member(PredURI, [bmpaf:'hasParticipantA', 
                    bmpaf:'hasParticipantB']),
    readInTargetGraph(Event, PredURI, Subject).
    
queryInteraction(Event, IntClassShort) :-
    mostSpecificType(Event, IntClass, 'http://purl.bioontology.org/net/brunel/paf#Event'),
    rdf_global_id(IntClassShort, IntClass).
    
queryModification(Event, ModClassShort) :-
    readInTargetGraph(Event, bmpaf:'hasModificationFeature', Modification),
    mostSpecificType(Modification, ModClass, 'http://purl.bioontology.org/net/brunel/paf#ProteinModification'),
    rdf_global_id(ModClassShort, ModClass).
    
participant(Subject, PredURI) :-
    participantId(PredURI, JType, JText, JId),
    card(JType, Part),
    participantType(Part, Class),
    participantMatch(Subject, Class, JText, JId).

participantMatch(Subject, Class, JText, JId) :-
    card(JId, PartId), 
    atomic_list_concat(IdList, '|', PartId),
    createClassedMultiIdObject('participant', Class, Subject, bmpaf:'hasEntityId', IdList), !,
    findall(_, participantInfo(Subject, JText, text), _).

participantMatch(Subject, Class, JText, JId) :-
    \+card(JId, _), 
    card(JText, Text), 
    createClassedIdObject('participant', Class, Subject, bmpaf:'hasEntityText', Text),
    \+readInTargetGraph(Subject, bmpaf:'hasEntityId', _), !.

participantMatch(Subject, Class, JText, JId) :-
    \+card(JId, _), 
    card(JText, _), 
    createClassedObject('participant', Class, Subject),
    findall(_, participantInfo(Subject, JText, text), _), !.

participantMatch(Subject, Class, JText, JId) :-
    \+card(JId, _), 
    \+card(JText, _), !,
    createClassedObject('participant', Class, Subject).

participantInfo(Subject, JText, text) :-
    card(JText, Text),
    insertDataValue(Subject, bmpaf:'hasEntityText', Text).

participantType('protein', bmpaf:'Protein').
participantType('chemical', bmpaf:'Chemical').
participantType('protein_family', bmpaf:'ProteinFamily').
participantType('gene', bmpaf:'Gene').
participantType('complex', bmpaf:'Complex').

participantId(bmpaf:'hasParticipantA', '/extracted_information/participant_a/entity_type', '/extracted_information/participant_a/entity_text', '/extracted_information/participant_a/identifier').
participantId(bmpaf:'hasParticipantB', '/extracted_information/participant_b/entity_type', '/extracted_information/participant_b/entity_text', '/extracted_information/participant_b/identifier').

interaction(IntClass) :-
    card('/extracted_information/interaction_type', IntType),
    interactionClass(IntType, IntClass).
    
interactionClass('binds', bmpaf:'Binding').
interactionClass('increases', bmpaf:'PositiveRegulation').
interactionClass('decreases', bmpaf:'NegativeRegulation').
interactionClass('adds_modification', bmpaf:'AddsModification').
interactionClass('inhibits_modification', bmpaf:'InhibitsModification').
interactionClass('translocates', bmpaf:'Translocation').
interactionClass('increases_activity', bmpaf:'IncreasesActivity').
interactionClass('decreases_activity', bmpaf:'DecreasesActivity').

modification(ModClass) :- 
    card(Key, ModType),
    string_concat(_, 'modification_type', Key),
    modificationType(ModType, ModClass).
        
modificationType('phosphorylation', bmpaf:'Phosphorylation').
modificationType('acetylation', bmpaf:'Acetylation').
modificationType('farnesylation', bmpaf:'Farnesylation').
modificationType('glycosylation', bmpaf:'Glycosylation').
modificationType('hydroxylation', bmpaf:'Hydroxylation').
modificationType('methylation', bmpaf:'Methylation').
modificationType('ribosylation', bmpaf:'Ribosylation').
modificationType('sumoylation', bmpaf:'Sumoylation').
modificationType('ubiquitination', bmpaf:'Ubiquitination').


%%%%%%%%%%%%%%%%%%

evidence(Statement) :-
    card('/evidence', Evidence), 
    insertDataValue(Statement, bmpaf:'hasTextualEvidence', Evidence).

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

%%%%%%%%%%%%%%%%%%%%
% generic predicates
%%%%%%%%%%%%%%%%%%%


insertDataValue(Subject, Predicate, Value) :-
    makeLiteral(Value, Literal),
    writeInTargetGraph(Subject, Predicate, Literal).    

createClassedIdObject(_, Class, Subject, KeyId, Id) :-
    normalize_space(atom(ValueId), Id),
    makeLiteral(ValueId, Literal),
    identifyObject(Subject, Class, KeyId, Literal), !.    
    
createClassedIdObject(UriBase, Class, Subject, KeyId, Id) :-
    normalize_space(atom(ValueId), Id),
  %  makeLiteral(ValueId, Literal),
    createClassedObject(UriBase, Class, Subject),
    insertDataValue(Subject, KeyId, ValueId), !.
        
identifyObject(Subject, Class, KeyId, LitValueId) :-
    readInTargetGraph(Subject, rdf:'type', Class),
    readInTargetGraph(Subject, KeyId, Literal),
    rdf_literal_value(Literal, Value),
    rdf_literal_value(LitValueId, Value2),
    \+Value2='',
    upperCase(Value, ValUpper),
    upperCase(Value2, ValUpper).

createClassedMultiIdObject(_, Class, Subject, KeyId, IdList) :-
    identifyMultiIdObject(Subject, Class, KeyId, IdList), !,
    assertIdentifiers(Subject, KeyId, IdList).

createClassedMultiIdObject(UriBase, Class, Subject, KeyId, IdList) :-
    createClassedObject(UriBase, Class, Subject),
    assertIdentifiers(Subject, KeyId, IdList), !.

identifyMultiIdObject(Subject, Class, KeyId, IdList) :-
    readInTargetGraph(Subject, rdf:'type', Class),
    readInTargetGraph(Subject, KeyId, Literal),
    rdf_literal_value(Literal, Value),
    member(Id, IdList),
    normalize_space(atom(ValueId), Id),
    upperCase(Value, ValUpper),
    upperCase(ValueId, ValUpper).

assertIdentifiers(_, _, []) :- !.

assertIdentifiers(Subject, KeyId, [First|Rest]) :-
    normalize_space(atom(ValueId), First),
    insertDataValue(Subject, KeyId, ValueId),
    assertIdentifiers(Subject, KeyId, Rest).


createClassedObject(UriBase, Class, Subject) :- !,
    createFreshObject(UriBase, Subject),
    writeInTargetGraph(Subject, rdf:'type', Class).
    
createFreshObject(UriBase, Subject) :-
    uuid(U), !, 
    atomic_list_concat([UriBase, '_', U], Subject).


%%%%%%%%%%%%%%
%%  readerTypes
%%%%%%%%%%%%%%%%

assertReader(Reader, 'human') :- 
    !,
    writeInTargetGraph(Reader, rdf:'type', bmpaf:'Human').

assertReader(Reader, 'machine') :-  
    !,
    writeInTargetGraph(Reader, rdf:'type', bmpaf:'Computer').

assertReader(Reader, ReadType) :- 
    member(ReadType, ['human_machine', 'human-machine']),
    !,
    writeInTargetGraph(Reader, rdf:'type', bmpaf:'Human'),
    writeInTargetGraph(Reader, rdf:'type', bmpaf:'Computer').
    


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

%%%%%%%%%%%%

writeInTargetGraph(X, Y, Z) :-
    targetGraph(Graph),
    rdf_global_id(X, Xg),
    rdf_global_id(Y, Yg),
    rdf_global_id(Z, Zg),
    rdf_assert(Xg, Yg, Zg, Graph).

readInTargetGraph(X, Y, Z) :-
    targetGraph(Graph),
    rdf_global_id(X, Xg),
    rdf_global_id(Y, Yg),
    rdf_global_id(Z, Zg),
    rdf(Xg, Yg, Zg, Graph).

addMetaData(Graph) :-
    rdf_assert(bm:graph, rdf:'type', void:'Dataset', Graph),
    rdf_assert(bm:graph, rdf:'type', prov:'Entity', Graph),
    insertDataValue(bm:graph, dc:'creator', 'Szymon Klarman'),
    rdf_assert(bm:graph, prov:'wasGeneratedBy', bm:'assemblyActivity', Graph),
    date(date(Y, M, D)), atomic_list_concat([Y, M, D], '-', Date),
    insertDataValue(bm:graph, dc:'title', 'A dataset containing biological events for Big Mechanism'),
    insertDataValue(bm:graph, rdfs:comment, 'This dataset has been generated from the data extracted by NaCTeM over a selected corpus of PubMed papers, and further enriched with information available online (publication metadata, chemicals/genes).'),
    insertDataValue(bm:graph, dc:'date', Date).
    %rdf_assert(bm:graph, owl:'imports', 'http://purl.bioontology.org/net/brunel/paf#', Graph),
    %rdf_assert(bm:graph, owl:'imports', 'http://purl.bioontology.org/net/brunel/p#', Graph).


%%%%%%%%%%%%%%%%%%%%%%



makeLiteral(@false, literal(lang(en, 'false'))) :- !.
makeLiteral(@true, literal(lang(en, 'true'))) :- !.
makeLiteral(X, literal(type('http://www.w3.org/2001/XMLSchema#int', X))) :- 
    integer(X), !.
makeLiteral(X, literal(type('http://www.w3.org/2001/XMLSchema#decimal', X))) :- 
    number(X), \+integer(X), !.
makeLiteral(Value, literal(lang(en, Value))) :- !.

upperCase(Value, ValUpper) :-
    string_upper(Value, ValueUpper),
    atom_string(ValUpper, ValueUpper).

negation(@false, @true).
negation(@true, @false).

%%%%%%%

saveGraph(Graph) :- 
    file_search_path(my_home, Dir),
    atomic_list_concat([Dir, '/', Graph, '.rdf'], OutputPath),
    rdf_save(OutputPath, [graph(Graph), encoding(utf8)]), !.


%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPARQL-based Id resolution
%%%%%%%%%%%%%%%%%%%%%%%%%

sparql_setup('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> <http://purl.uniprot.org/core/mnemonic> ?x}', 'sparql.uniprot.org', '/sparql/').
%sparql_setup('UNIPROT:', 'SELECT ?x WHERE {<http://purl.uniprot.org/uniprot/', '> rdfs:label ?x}', 'sparql.uniprot.org', '/sparql/').
sparql_setup('CHEBI:', 'SELECT ?x WHERE {<http://purl.obolibrary.org/obo/CHEBI_', '> rdfs:label ?x}', 'chebi.bio2rdf.org', '/sparql').
sparql_setup('HGNC:', 'SELECT ?x WHERE { <http://bio2rdf.org/hgnc:', '> <http://bio2rdf.org/hgnc_vocabulary:approved-symbol> ?x}', 'hgnc.bio2rdf.org', '/sparql').

identifierResolution(ProtID, Name) :-
    upperCase(ProtID, ProtIDupper),
    sparql_setup(Prefix, QueryFront, QueryBack, Host, Path),
    string_concat(Prefix, Number, ProtIDupper),
    atomic_list_concat([QueryFront, Number, QueryBack], Query),
    sparql_query(Query, row(Literal), [host(Host), path(Path), status_code(_)]),
    rdf_literal_value(Literal, Name), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% journals
%%%%%%%%%%%%%%%%%%%%



articlePublicationInfo :-
    rdf(X, rdf:'type', bmpaf:'JournalArticle'), 
    rdf(X, bmpaf:'hasPmcId', LiteralPmcId), 
    rdf_literal_value(LiteralPmcId, PmcId),
    pubmedJournalTitleYear(PmcId, Title, Year),
    createClassedIdObject('journal', bmpaf:'Journal', Journal, bmpaf:'hasTitle', Title),
    writeInTargetGraph(X, bmpaf:'publishedIn', Journal),
    insertDataValue(X, bmpaf:'hasPublicationYear', Year).

pubmedJournalTitleYear(PmcId, Title, Year) :- 
    atomic_list_concat(['http://www.ncbi.nlm.nih.gov/pmc/utils/ctxp/?ids=', PmcId, '&report=citeproc&format=json'], Query),
    http_open(Query, In, []), 
    json_read(In, json(X)), 
    member('container-title'= Title, X), !,
    member(issued=json(['date-parts'=[[Year | _]]]), X), !.

run_journal :-
    writeln('Exact matching...'), nl,
    findall(_, match(_, _), _),
    writeln('Soft matching...'), nl,
    findall(_, soft_match, _),
    writeln('Choose the right match: Space - confirm match; Enter - skip this journal.'), nl,
    findall(_, top_match(_, _, _), _),
    writeln('Asserting statement probabilities...'),
    findall(_, assertProbability, _).

match(X, Sjr) :-
	journalS(X, Val),
	journalT(Y, Val),
    rdf(Y, nov:'sjr', Sjr),
    assertScore(X, Sjr).

assertScore(X, Sjr) :-
    rdf_literal_value(Sjr, No),
    atom_number(No, Num),
    insertDataValue(X, bmp:'hasSJRscore', Num).

assertProbability :- 
    rdf(X, bmp:'hasSJRscore',  literal(type('http://www.w3.org/2001/XMLSchema#decimal', No))),
    NormalSjr is sqrt(round((No / 40) * 1000) / 1000),
    rdf(Art, bmpaf:'publishedIn', X),
    createClassedObject('probability', bmp:'ProbabilityRelevantToDocumentProvenance', Prob),
    insertDataValue(Prob, bmp:'hasProbabilityLevel', NormalSjr),
    rdf(Stat, bmpaf:'isExtractedFrom', Art),    
    writeInTargetGraph(Stat, bmp:'hasProvenanceProbability', Prob),
    writeInTargetGraph(Art, bmp:'hasProvenanceProbability', Prob).

journalS(X, Val) :-
	rdf(X, rdf:'type', bmpaf:'Journal'),
	rdf(X, rdfs:'label', Y),
	rdf_literal_value(Y, ValDiv),
	string_upper(ValDiv, Val).

journalT(X, Val) :-
	rdf(X, rdf:'type', nov:'Journal'),
	rdf(X, rdfs:'label', Y),
	rdf_literal_value(Y, ValDiv),
	string_upper(ValDiv, Val).

soft_match :- 
    rdf(X, rdf:'type', bmpaf:'Journal'), 
    \+rdf(X, bmp:'hasSJRscore', _),
    journalS(X, Val1), 
    journalT(Y, Val2), 
    split_string(Val1, " ", "", L1), 
    split_string(Val2, " ", "", L2), 
    list_to_set(L1, S1), 
    list_to_set(L2, S2), 
    intersection(S1, S2, Int), 
    union(S1, S2, Un),
    length(Int, N1), 
    length(Un, N2), 
    M is N1 / N2,
    M > 0,
    assert(match(X, Y, M)).
    
orderSolutions(X, SortedList) :- 
    findall([M, Y], match(X, Y, M), List),
    sort(0,  @>=, List,  SortedList).
    
top_match(Val1, Val2, M) :-
    rdf(X, rdf:'type', bmpaf:'Journal'), 
    \+matched(X),
    orderSolutions(X, SortedList),
    pick_next(X, SortedList, [M, Y]),
    journalS(X, Val1), 
    journalT(Y, Val2),
    writeln(Val1),
    writeln(Val2),
    writeln(M),
    get_single_char(Code),
    affirm(Code, X, Y).

pick_next(X, _, _) :- 
    matched(X), !.

pick_next(X, SortedList, El) :-
    nth1(_, SortedList, El),
    \+matched(X).

affirm(32, X, Y) :- 
    rdf(Y, nov:'sjr', Sjr),
    assertScore(X, Sjr),
    asserta(matched(X)),
    writeln('Match confirmed.'), nl.
    
affirm(13, X, _) :- 
    asserta(matched(X)),
    writeln('Journal skipped.'), nl.

affirm(Code, _, _) :-
    \+Code = 32,
    \+Code = 97,
    \+Code = 13.





