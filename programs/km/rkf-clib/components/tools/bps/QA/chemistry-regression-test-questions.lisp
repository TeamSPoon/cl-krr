;;
;; $Id: chemistry-regression-test-questions.lisp,v 1.8 2006/05/18 20:21:41 jfan Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(setq *CHEMISTRY-TEST-QUESTION-LIST* 
'(
		   "A mixture is the result of a mix.
                    The volume of an h2o substance is 2 liters.
                    The volume of an nh3 substance is 3 liters.
                    The h2o substance is the first reactant of the mix.
                    The nh3 substance is the second reactant of the mix.
                    What is the volume of the mixture?"

                     "A mixture is the result of a mix.
                      The mixture has an h2o substance.
                      The mixture has an nh3 substance.
                      The volume of the h2o substance is 2 liters.
                      The volume of the nh3 substance is 3 liters.
                      The h2o substance is the first reactant of the mix.
                      The nh3 substance is the second reactant of the mix.
                      What is the volume of the mixture?"

		     "A H2 Substance and A O2 Substance react. 
                      The result of the reaction is a H2O Substance. 
                      What is the chemical equation of the reaction?"

		     "A hydrogen and an oxygen react.
                      what is the result of the reaction?"

		     "A H2 Substance and a O2 Substance react. 
                      What is the result of the reaction?"

		     "A H2 and a O2 react. 
                      What is the result of the reaction?"

		     "A CH4 Substance and a O2 Substance react. 
                      The first result of the reaction is a CO2 Substance. 
                      The second result of the reaction is a H2O Substance. 
                      What is the chemical equation of the reaction?"

		     "A Na Substance and a H2O Substance react. 
                      The first result of the reaction is a NaOH Substance. 
                      The second result of the reaction is a H2 Substance. 
                      What is the chemical equation of the reaction?"

		     "A Fe Substance and a O2 Substance react. 
                      The result of the reaction is a Fe2O3 Substance. 
                      What is the chemical equation of the reaction?"

                     "A Fe and a O2 react. What is the result of the reaction?"

			"What is a combination reaction?"

			"A metal and a nonmetal react. What is the result of reaction?"

			"What is the example of a combination reaction?"

			"What is a decomposition reaction?"

			"A combustion in air with raw material propane. 
                         What is the chemical equation of the combustion in air?"

			"A combustion in air with raw material methanol. 
                         What is the chemical equation of the combustion in air?"

			"What is a solution?"
			"What is an aqueous solution?"
			"What is the conductivity of a salt solution?"
			"What is the conductivity of a aqueous solution of molecular substance?"
			"What is an electrolyte?"
			"What is a nonelectrolyte?"
			"What is a strong electrolyte?"
			"What is a weak electrolyte?"
			"What is a salt substance?"
			"What is the difference between a strong electrolyte and a weak electrolyte?"
			"What is a precipitation reaction?"
			"What is a metathesis reaction?"
			"What is an acid?"
			"What is the example of an acid?"
			"What is a base?"
			"What is the example of a base?"
			"What is a monoprotic acid?"
			"What is a diprotic acid?" 
			"What is a neutralization reaction?"
			"What is the result of a combustion in air?"
			"What is the result of a combination reaction?"
			"A combustion in air with raw material propane. What is the chemical equation of the combustion in air?"
			"A propane reacts. The reaction is a combustion reaction. What is the chemical equation of the reaction?"
			"There is an ionic compound substance.
                         The ionic compound substance has a h plus substance.
                         What are the parts of the ionic compound substance?"

"\"H_2 + O_2 --> H_2O\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"\"CH_4 + O_2 --> CO_2 + H_2O\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"\"Na(s) + H_2O(l) --> NaOH(aq) + H_2\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"\"Fe(s) + O_2 --> Fe_2O_3(s)\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"\"Fe + O_2 --> Fe_2O_3\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"\"C_2H_4(g) + O_2(g) --> CO_2(g) + H_2O(g)\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"\"Al(s) + HCl(aq) --> AlCl_3(aq) + H_2(g)\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"\"O_2 + NO --> NO_2\" is an equation of a reaction.
What is the balanced equation of the reaction?"

"A calcium carbonate decomposes.
What is the result of the decomposing?"

"A sodium azide decomposes.
What is the result of the decomposing?"

"A lithium metal reacts with a fluorine gas.
The reaction equals a combination reaction.
What is the chemical equation of the reaction?"

"A barium carbonate is being heated.
The heating produces a combination reaction.
What is the chemical equation of the combination reaction?"

"A aluminum metal reacts with a oxygen.
The reaction equals a combination reaction.
What is the chemical equation of the combination reaction?"

"A mercury sulfide is decomposing.
The decomposing is a decomposition reaction.
What is the chemical equation of the decomposition reaction?"

"A methanol and an oxygen are being burned.
The burning equals a reaction.
What is the chemical equation of the reaction?"

"Is it true that the conductivity of a salt solution is positive?"

"Is it true that the conductivity of a solution of sugar is a positive?"

"Is it true that \"NaCl\" is a strong electrolyte?"

"There are 6 cations in a solution of \"NiSO_4\".
How many anions are in the solution?"

"\"ZnCl_2\" is a strong electrolyte.
\"ZnCl_2\" is dissociating in water.
What is the chemical equation of the dissociating?"

"\"HNO_3\" is a strong electrolyte.
\"HNO_3\" is dissociating in water.
What is the chemical equation of the dissociating?"

"\"K_2SO_4\" is a strong electrolyte.
\"K_2SO_4\" is dissociating in water.
What is the chemical equation of the dissociating?"

"\"Ca(OH)_2\" is a strong electrolyte.
\"Ca(OH)_2\" is dissociating in water.
What is the chemical equation of the dissociating?"

"A small amount of sodium is dissolved in a large amount of water.
The result of the dissolving is a solution.
What is the solvent of the solution?
What is the solute of the solution?"

"0.005 liter of NaCl is dissolved in water.
A solution is the result of the dissolving.
0.005 liter of sugar is dissolved in a 2nd water.
A 2nd solution is the result of the dissolving.
What is the difference between the 1st solution and the 2nd solution?"

"Is it true that a salt substance equals an ionic compound?"

"Is it true that a salt substance equals a molecular compound?"

"A H2 and a O2 react into a H2O. 
What is the chemical equation of the reaction?"

"A H2 and a O2 react. 
The result of the reaction is H2O. 
What is the chemical equation of the reaction?"

"A Na and a H2O react.
The first result of the reaction is a NaOH.
The second result of the reaction is a H2.
What is the chemical equation of the reaction?"

))