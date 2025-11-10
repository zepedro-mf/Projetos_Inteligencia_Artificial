% =========================================
% MÓDULO P1 — DOMÍNIO (Pacientes, Consultas, Tensões)
% =========================================


%=========================================================================

%                                 FAKE DATA


% patient(ID, Name, Age, Sex, City)
% Random data about patients to be called later
patient(p1, 'Alice Silva', 35, female, 'Lisbon').
patient(p2, 'Bruno Costa', 52, male, 'Porto').
patient(p3, 'Carla Mendes', 68, female, 'Coimbra').
patient(p4, 'Daniel Rocha', 41, male, 'Braga').
patient(p5, 'Eva Martins', 27, female, 'Faro').
patient(p6, 'Filipe Sousa', 59, male, 'Aveiro').
patient(p7, 'Gabriela Pinto', 73, female, 'Setubal').
patient(p8, 'Hugo Almeida', 46, male, 'Leiria').
patient(p9, 'Ines Carvalho', 31, female, 'Viseu').
patient(p10, 'Joao Ferreira', 64, male, 'Evora').
% (add more later up to 10)

% consultation(ID, Date, PatientID, Doctor, Systolic, Diastolic)
% ID = the consultation id itself
% Date = date of the consultation
% Doctor = who performed the consultation
% S, D and P are measurements taken during the consultation
% =========================================
% Consultation Data (20 total)
% =========================================

consultation(c1,  '2025-03-10', p1, dr_silva, 120, 80, 72).   
consultation(c2,  '2025-06-15', p1, dr_silva, 135, 85, 75).   
consultation(c3,  '2025-03-11', p2, dr_mendes, 150, 95, 85).  
consultation(c4,  '2025-09-12', p2, dr_mendes, 145, 90, 82).  
consultation(c5,  '2025-03-12', p3, dr_lima, 90, 55, 70).     
consultation(c6,  '2025-07-20', p3, dr_lima, 100, 65, 74).    
consultation(c7,  '2025-03-15', p4, dr_mendes, 130, 85, 76).  
consultation(c8,  '2025-04-18', p4, dr_silva, 160, 100, 82).  
consultation(c9,  '2025-03-18', p5, dr_silva, 115, 78, 70).   
consultation(c10, '2025-05-22', p5, dr_oliveira, 118, 79, 71).
consultation(c11, '2025-03-20', p6, dr_lima, 162, 102, 88).   
consultation(c12, '2025-07-25', p6, dr_silva, 155, 95, 83).   
consultation(c13, '2025-10-02', p6, dr_mendes, 138, 89, 79).  
consultation(c14, '2025-03-22', p7, dr_mendes, 145, 92, 88).  
consultation(c15, '2025-09-05', p7, dr_mendes, 142, 91, 90).  
consultation(c16, '2025-03-25', p8, dr_silva, 105, 68, 65).  
consultation(c17, '2025-07-30', p8, dr_lima, 98, 58, 60).     
consultation(c18, '2025-03-26', p9, dr_lima, 95, 55, 60).     
consultation(c19, '2025-06-01', p9, dr_silva, 102, 66, 68).   
consultation(c20, '2025-03-30', p10, dr_mendes, 138, 85, 78). 


%==========================================================================

% classification(Type, Systolic, Diastolic)
% Uses the measurements to check if they fit within certain criteria
% to determine what type of blood pressure you have
classification(hypotension, S, D) :- S < 100; D < 60.
classification(normal, S, D) :- S >= 100, S < 140, D >= 60, D < 90.
classification(hypertension, S, D) :- S >= 140; D >= 90.

% coherence rule -> to make sure there weren't any significant mistakes during measuring
coherent(S, D) :- S > D.

% classify consultation
% to find out the result type of a specific consultation
classify_consultation(ID, Category) :-
    consultation(ID, _, _, _, S, D),
    classification(Category, S, D).

% valid consultation (systolic > diastolic)
valid_consultation(ID) :-
    consultation(ID, _, _, _, S, D),
    coherent(S, D).

% Find all consultations by a given patient
% consultations_by_patient('Alice Silva', ID).
consultations_by_patient(PatientName, ConsultationID) :-
    patient(PID, PatientName, _, _, _),
    consultation(ConsultationID, _, PID, _, _, _).

%  Find all consultations by a specific doctor
%  consultations_by_doctor(dr_mendes, ID).
consultations_by_doctor(Doctor, ConsultationID) :-
    consultation(ConsultationID, _, _, Doctor, _, _).

% Find all patients within a certain age range
% patients_in_age_range(30, 50, Name).
patients_in_age_range(MinAge, MaxAge, Name) :-
    patient(_, Name, Age, _, _),
    Age >= MinAge,
    Age =< MaxAge.


% NOW THESE ARE THINGS I'LL NEED TO ASK PROLOG TO GET SPERCIFY FUNCTION? ANSWER?

%   Show all patients =================>     consultation(ID, Date, PID, Doctor, S, D, P).	
%   Classify one consultation ========>	     classify_consultation(c1, Category).	
%   Classify all consultation ========>	     classify_consultation(ID, Category).	
%   Validate readings	================>    valid_consultation(ID).	
%   Find consultations by patient ====> 	 consultations_by_patient('Alice Silva', ID).	
%   Find consultations by doctor	====>    consultations_by_doctor(dr_mendes, ID).	
%   Patients by age range	============>    patients_in_age_range(30, 60, Name).