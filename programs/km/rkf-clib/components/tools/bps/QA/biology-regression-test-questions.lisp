;;
;; $Id: biology-regression-test-questions.lisp,v 1.6 2006/11/02 22:49:31 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(setq *BIOLOGY-TEST-QUESTION-LIST* 
'(
  "What are the parts of a cell?"

  "What are the similarities between an animal cell and a plant cell?"

  "What are the differences between an animal cell and a plant cell?"
				     
  "What is a chromatin?"

  "What is a chromosome?"

  "What is a ribosome?"
				     
  "What is a free ribosome?"
				     
  "What is a bound ribosome?"

  "What are the similarities between a free ribosome and a bound ribosome?"
				      
  "What is a vesicle?"

  "What is the difference between smooth endoplasmic reticulum and rough endoplasmic reticulum?"

  "What is the difference between a ribosome and a bound ribosome?"

  "What are the elements of an endomembrane system?"
  "What are the parts of a plant cell?"

  "What are the parts of an animal cell?"
				     
  "What are the parts of a prokaryotic cell?"

  "What is the structure of a cell?"

  "What is the function of a plasma membrane?"

  "What is the location of cytosol?"

  "What is the location of genes in a cell?"

  "What is a mitosis?"

  "How many chromosomes are in a person's cell?"

  "What is the agent of a protein synthesis?"

  "What is the function of a rough endoplasmic reticulum?"

  "What is the function of a golgi apparatus?"

  "There is a bound ribosome.
   Is it true that the bound ribosome is enclosed in the membrane of the bound ribosome?"

  "Is it true that there is a difference between the structure of a bound ribosome and the structure of a free ribosome?"

  "Is it true that bound ribosomes synthesize membrane proteins and secreted proteins?"

  "Is it true that the location for a bound ribosome equals the cytoplasmic surface of a plasma membrane?"

  "Is it true that bound ribosomes are inside the spaces inside the rough endoplasmic reticulum?"

  "Is it true that a nuclear envelope is part of the membranes?"

  "Is it true that a chloroplast is part of the membranes?"

  "Is it true that a Golgi apparatus is part of the membranes?"

  "Is it true that a plasma membrane is part of the membranes?"

  "Is it true that an endoplasmic reticulum is part of the membranes?"

  "Is it true that a chloroplast is part of a plant cell?"

  "Is it true that a chloroplast is part of an animal cell?"

  "There is a wall.
   The material of the wall equals cellulose.
   Is it true that the wall is part of a plant cell?"

  "There is a wall.
   The material of the wall equals cellulose.
   There is a 2nd wall.
   The material of the 2nd wall equals cellulose.
   Is it true that the 2nd wall is part of an animal cell?"

  "Is it true that a centriole is part of a plant cell?"

  "Is it true that a centriole is part of an animal cell?"

  "Is it true that a ribosome is part of a prokaryote?"

  "Is it true that a nuclear envelope is part of a prokaryote?"

  "Is it true that a chloroplast is part of a prokaryote?"

  "Is it true that an endoplasmic reticulum is part of a prokaryote?"

  "Is it true that there are numerous lysosomes in a cell of a muscle?"

  "Is it true that there are numerous lysosomes in a cell of a nerve?"

  "There is a white cell in blood.
   The cell is from a phagocyte.
   Is it true that there are numerous lysosomes in the cell?"

  "A cell is part of a leaf of a plant.
   Is it true that there are numerous lysosomes in the cell?"

  "Is it true that there are numerous lysosomes in a bacterium cell?"

  "The amount of cytoskeletons in a prokaryote is zero.
   A compartment is part of an organelle.
   Is it true that the organelle is part of a eukaryotic cell?"

  "The amount of cytoskeletons in a prokaryote is zero.
   A compartment is part of an organelle.
   A 2nd compartment is part of a 2nd organelle.
   Is it true that the 2nd organelle is part of a prokaryote?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that the cytoplasm in a eukaryotic cell streams?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that the cytoplasm in a prokaryote streams?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that eukaryotes move?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that prokaryotes move?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that the diameter of a eukaryotic cell is less than 10 mm?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that the diameter of a prokaryote is less than 10 mm?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that a eukaryotic cell concentrates the genes in a region?"

  "The amount of cytoskeletons in a prokaryote is zero.
   Is it true that a prokaryote concentrates the genes in a region?"

  "Is it true that the function of a nucleolus is producing ribosomes?"

  "Is it true that the function of a lysosome is digestion in a cell?"

  "Is it true that the function of a ribosome is protein synthesis?"

  "Is it true that the function of a Golgi apparatus is the secretion of cell products?"

  "Is it true that the function of microtubules is muscle contraction?"

  "A cell contacts some cyanide.
   A molecule is part of the cell.
   The molecule produces Adenosine triphosphate.
   The cyanide binds with the molecule.
   Is it true that the cyanide is inside the mitochondria of the cell?"

  "A cell contacts some cyanide.
   A molecule is part of the cell.
   The molecule produces Adenosine triphosphate.
   The cyanide binds with the molecule.
   Is it true that the cyanide is inside the ribosomes of the cell?"

  "A cell contacts some cyanide.
   A molecule is part of the cell.
   The molecule produces Adenosine triphosphate.
   The cyanide binds with the molecule.
   Is it true that the cyanide is inside the peroxisomes of the cell?"

  "A cell contacts some cyanide.
   A molecule is part of the cell.
   The molecule produces Adenosine triphosphate.
   The cyanide binds with the molecule.
   Is it true that the cyanide is inside the lysosomes of the cell?"

  "A cell contacts some cyanide.
   A molecule is part of the cell.
   The molecule produces Adenosine triphosphate.
   The cyanide binds with the molecule.
   Is it true that the cyanide is inside the endoplasmic reticulum of the cell?"

  "A tiny virus penetrates the membrane of a cell of a plant.
   The virus enters the cell.
   The virus creates a second virus.
   The second virus spreads inside the plant.
   The second virus penetrates zero membranes.
   What is the method of the spreading?"

  "What are differences between a mitochondrion and a chloroplast?"

  "What are differences between a mitochondrion and a ribosome?"

  "What are differences between a mitochondrion and a lysosome?"

  "What are differences between a mitochondrion and a peroxisome?"

  "What are differences between a chloroplast and a ribosome?"

  "What are differences between a chloroplast and a lysosome?"

  "What are differences between a chloroplast and a peroxisome?"

  "What are differences between a ribosome and a lysosome?"

  "What are differences between a ribosome and a peroxisome?"

  "What are differences between a lysosome and a peroxisome?"

  "What is the relationship between a chromosome and a chromatin?"

  "What are three functions of smooth endoplasmic reticulum?"

  "There are some organelles.
   The function of the organelles is energy conversion.
   What are the organelles?"

"Transport vesicles integrate the membrane system in a cell.
What is the method of the integrating?"

"There is a component of a cytoskeleton.
The component's function equals anchoring the nucleus.
What is the component?"

"There is a component of a cytoskeleton.
The component's function is guiding. 
The component guides transport vesicles. 
The vesicles are being guided from the Golgi apparatus.
The vesicles are being guided to the plasma membrane.
What is the component?"

"There is a component of a cytoskeleton.
The component's function is the cell's amoeba movement.
What is the component?"

"Some similar cells are elements of a group.
What are differences between a tissue and the group?"

"What are the steps of mitosis?"

"What are the steps of an Interphase event?"

"Mitosis is a step of the event of a cell cycle.
What is the difference between the duration of mitosis and the duration of the event?"
  ))