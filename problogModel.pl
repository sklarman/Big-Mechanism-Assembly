%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProbLog model and parameters 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% registering given probability values and fixing defaults
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modelMatchProb(x, x, 0).
modelMatchProbDefined(Ev) :- modelMatchProb(Ev, _, _).
P::modelMatchProb(Ev, G) :- modelMatchProb(Ev, G, P).
0::modelMatchProb(Ev) :- \+modelMatchProbDefined(Ev).

textProb(x, 0).
textProbDefined(Stat) :- textProb(Stat, _). 
1::textProb(Stat) :- textProb(Stat, certain).
0.8::textProb(Stat) :- textProb(Stat, uncertain).
1::textProb(Stat) :- \+textProbDefined(Stat).

extractionProb(x, 0).
extractionProbDefined(Stat) :- extractionProb(Stat, _). 
P::extractionProb(Stat) :- extractionProb(Stat, P).
0.6::extractionProb(Stat) :- \+extractionProbDefined(Stat).

provenanceProb(x, 0).
provenanceProbDefined(Doc) :- provenanceProb(Doc, _). 
P::provenanceProb(Doc) :- provenanceProb(Doc, P).
0.1::provenanceProb(Doc) :- \+provenanceProbDefined(Doc).

experimentProb(x, 0).
experimentProbDefined(Ev) :- experimentProb(Ev, _).
0.95::experimentProb(Ev) :- experimentProb(Ev, true).
0.2::experimentProb(Ev) :- experimentProb(Ev, false).
0.5::experimentProb(Ev) :- \+experimentProbDefined(Ev).

representingStatement(x, x, x, x).
somePositiveSupport(Ev) :- representingStatement(Ev, _, true, _).
someNegativeSupport(Ev) :- representingStatement(Ev, _, false, _).
0::positiveSupport(Ev) :- \+somePositiveSupport(Ev).
0::negativeSupport(Ev) :- \+someNegativeSupport(Ev).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% weighing probabilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


1::textProbWeighted(Stat) :- textProb(Stat).
0.76650623::textProbWeighted(Stat).
1::provenanceProbWeighted(Stat) :- provenanceProb(Stat).
0.38908585::provenanceProbWeighted(Stat).
1::extractionProbWeighted(Stat) :- extractionProb(Stat).
0.9::extractionProbWeighted(Stat).
1::experimentProbWeighted(Stat) :- experimentProb(Stat).
0::experimentProbWeighted(Stat).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% computing supports, inconsistency, conflict and corroboration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


0.9::positiveDocumentSupport(Ev, Doc) :- representingStatement(Ev, Stat, true, Doc), textProbWeighted(Stat), extractionProbWeighted(Stat). 
0.9::negativeDocumentSupport(Ev, Doc) :- representingStatement(Ev, Stat, false, Doc), textProbWeighted(Stat), extractionProbWeighted(Stat). 

0.3::positiveSupport(Ev) :- positiveDocumentSupport(Ev, Doc), provenanceProbWeighted(Doc). 
0.3::negativeSupport(Ev) :- negativeDocumentSupport(Ev, Doc), provenanceProbWeighted(Doc).

positiveExpSupport(Ev) :- positiveSupport(Ev), experimentProbWeighted(Ev).
negativeExpSupport(Ev) :- negativeSupport(Ev), \+experimentProbWeighted(Ev).

internalInconsistency(Ev) :- event(Ev), positiveSupport(Ev), negativeSupport(Ev).

posCorroboration(Ev) :- event(Ev), modelMatchProb(Ev), positiveExpSupport(Ev), \+negativeExpSupport(Ev).
negCorroboration(Ev) :- event(Ev), \+modelMatchProb(Ev), \+positiveExpSupport(Ev), negativeExpSupport(Ev).

posConflict(Ev) :- event(Ev), modelMatchProb(Ev), \+positiveExpSupport(Ev), negativeExpSupport(Ev).
negConflict(Ev) :- event(Ev), \+modelMatchProb(Ev), positiveExpSupport(Ev), \+negativeExpSupport(Ev).

corroboration(Ev) :- posCorroboration(Ev).
corroboration(Ev) :- negCorroboration(Ev).

conflict(Ev) :- posConflict(Ev).
conflict(Ev) :- negConflict(Ev).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProbLog query
%%  
    
%query(positiveSupport(Ev)).
%query(negativeSupport(Ev)).

query(posCorroboration(Ev)).
query(negCorroboration(Ev)).
query(corroboration(Ev)).

query(posConflict(Ev)).
query(negConflict(Ev)).
query(conflict(Ev)).

query(internalInconsistency(Ev)).

% posCorroboration = event is in the model and is found to be true 
% negCorroborates = event is not in the model and is found to be false 
% posConflict = event is in the model but is found to be false  ==action==> remove from the model if the value is high
% negConflict = event is not in the model but is found to be true ==action==> add to the model if the value is high
