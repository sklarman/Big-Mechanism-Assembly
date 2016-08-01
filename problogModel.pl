
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProbLog model and parameters 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% registering given probability values and fixing defaults
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

groundProb(x, 0).
groundProbDefined(Stat) :- groundProb(Stat, _). 
P::groundProb(Stat) :- groundProb(Stat, P).
0.5::groundProb(Stat) :- \+groundProbDefined(Stat).

representingStatement(x, x, x, x, x).
somePositiveSupport(Ev) :- event(Ev), representingStatement(Ev, _, true, _, _).
someNegativeSupport(Ev) :- event(Ev), representingStatement(Ev, _, false, _, _).
0::positiveSupport(Ev) :- event(Ev), \+somePositiveSupport(Ev).
0::negativeSupport(Ev) :- event(Ev), \+someNegativeSupport(Ev).

experimentProb(x, 0).

trust(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% weighing probabilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


1::textProbWeighted(Stat) :- textProb(Stat).
0.76650623::textProbWeighted(Stat).
1::provenanceProbWeighted(Stat) :- provenanceProb(Stat).
0.38908585::provenanceProbWeighted(Stat).
1::extractionProbWeighted(Stat) :- extractionProb(Stat).
0.9::extractionProbWeighted(Stat).
1::groundProbWeighted(Stat) :- groundProb(Stat).
0.01::groundProbWeighted(Stat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% computing supports, inconsistency, likelihoods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

0.9::positiveStatementSupport(Ev, Doc, Sub) :- 
        representingStatement(Ev, Stat, true, Doc, Sub), 
        textProbWeighted(Stat), 
        extractionProbWeighted(Stat), 
        groundProbWeighted(Stat). 
        
0.9::negativeStatementSupport(Ev, Doc, Sub) :- 
        representingStatement(Ev, Stat, false, Doc, Sub), 
        textProbWeighted(Stat), 
        extractionProbWeighted(Stat), 
        groundProbWeighted(Stat). 
        
0.8::positiveSubmitterSupport(Ev, Doc) :- 
        positiveStatementSupport(Ev, Doc, Sub),
        trust(Sub). 
        
0.8::negativeSubmitterSupport(Ev, Doc) :- 
        negativeStatementSupport(Ev, Doc, Sub),
        trust(Sub). 

0.7::positiveDocumentSupport(Ev) :- 
        positiveSubmitterSupport(Ev, Doc), 
        provenanceProbWeighted(Doc). 
        
0.7::negativeDocumentSupport(Ev) :- 
        negativeSubmitterSupport(Ev, Doc), 
        provenanceProbWeighted(Doc).

0.6::positiveSupport(Ev) :-  
        positiveDocumentSupport(Ev).

0.8::positiveSupport(Ev) :-  
        experimentProb(Ev, true).

0.6::negativeSupport(Ev) :- 
        negativeDocumentSupport(Ev).
       
0.5::negativeSupport(Ev) :- 
        experimentProb(Ev, false).

internalInconsistency(Ev) :- 
        event(Ev),
        positiveSupport(Ev), 
        negativeSupport(Ev).

totalPositiveUncertainty(Ev) :- 
        event(Ev), 
        positiveSupport(Ev), 
        \+negativeSupport(Ev).
        
totalNegativeUncertainty(Ev) :- 
        event(Ev), 
        \+positiveSupport(Ev), 
        negativeSupport(Ev).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProbLog query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    

query(positiveSupport(Ev)).
query(negativeSupport(Ev)).

query(internalInconsistency(Ev)).

query(totalPositiveUncertainty(Ev)).
query(totalNegativeUncertainty(Ev)).
