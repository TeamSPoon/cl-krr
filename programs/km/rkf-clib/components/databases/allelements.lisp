;;
;; $Id: allelements.lisp,v 1.3 2005/06/21 15:24:32 jfan Exp $
;;

(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)

(defvar *allelements* '((periodic_table "  "
                         (atom "    " (name "Actinium") "    "
                               (atomic_weight "227") "    "
                               (atomic_number "89") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3470") "    "
                               (symbol "Ac") "    "
                               ((density units "grams/cubic centimeter")
                                "      10.07    ")
                               "    " (electron_configuration "[Rn] 6d1 7s2 ")
                               "    " (electronegativity "1.1") "    "
                               ((atomic_radius units "Angstroms") "1.88")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      22.5    ")
                               "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.12    ")
                               "    " (ionization_potential "5.17") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          12    ")
                               "  ")
                         "  "
                         (atom "    " (name "Aluminum") "    "
                               (atomic_weight "26.98154") "    "
                               (atomic_number "13") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "2740") "    "
                               ((melting_point units "Kelvin") "933.5") "    "
                               (symbol "Al") "    "
                               ((density units "grams/cubic centimeter")
                                "      2.7    ")
                               "    " (electron_configuration "[Ne] 3s2 p1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.18")
                               "    " (electronegativity "1.61") "    "
                               ((atomic_radius units "Angstroms") "1.43")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      290.8    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      10    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      10.7    ")
                               "    " (ionization_potential "5.986") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.9    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          237    ")
                               "  ")
                         "  "
                         (atom "    " (name "Americium") "    "
                               (atomic_weight "243") "    "
                               (atomic_number "95") "    "
                               (oxidation_states "6, 5, 4, 3") "    "
                               ((boiling_point units "Kelvin") "2880") "    "
                               ((melting_point units "Kelvin") "1449") "    "
                               (symbol "Am") "    "
                               ((density units "grams/cubic centimeter")
                                "      13.7    ")
                               "    " (electron_configuration "[Rn] 5f7 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               ((atomic_radius units "Angstroms") "1.84")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      20.8    ")
                               "    " (ionization_potential "6") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Antimony") "    "
                               (atomic_weight "121.757") "    "
                               (atomic_number "51") "    "
                               (oxidation_states "+/-3, 5") "    "
                               ((boiling_point units "Kelvin") "1860") "    "
                               ((melting_point units "Kelvin") "903.91")
                               "    " (symbol "Sb") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.69    ")
                               "    "
                               (electron_configuration "[Kr] 4d10 5s2 p3 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.4")
                               "    " (electronegativity "2.05") "    "
                               ((atomic_radius units "Angstroms") "1.59")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      67.97    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      18.4    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      19.83    ")
                               "    " (ionization_potential "8.641") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.207    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          24.3    ")
                               "  ")
                         "    "
                         ((atom state "GAS") "  !
  " (name "Argon") "    " (atomic_weight "39.948") "    " (atomic_number "18")
                          "    " ((boiling_point units "Kelvin") "87.45")
                          "    " ((melting_point units "Kelvin") "83.95")
                          "    " (symbol "Ar") "    "
                          ((density units "grams/cubic centimeter")
                           "      1.784    ")
                          "    " (electron_configuration "[Ne] 3s2 p6 ")
                          "    " ((covalent_radius units "Angstroms") "0.98")
                          "    " (electronegativity "0") "    "
                          ((atomic_radius units "Angstroms") "0.88") "    "
                          ((heat_of_vaporization units "kilojoules/mole")
                           "      6.506    ")
                          "    "
                          ((atomic_volume units "cubic centimeters/mole")
                           "      24.2    ")
                          "    "
                          ((heat_of_fusion units "kilojoules/mole")
                           "      1.188    ")
                          "    " (ionization_potential "15.759") "    "
                          ((specific_heat_capacity units
                            "Joules/gram/degree Kelvin")
                           "      0.52    ")
                          "    "
                          ((thermal_conductivity units
                            "Watts/meter/degree Kelvin")
                           "          0.0177    ")
                          "  ")
                         "  "
                         (atom "    " (name "Arsenic") "    "
                               (atomic_weight "74.9216") "    "
                               (atomic_number "33") "    "
                               (oxidation_states "+/-3, 5") "    "
                               ((boiling_point units "Kelvin") "876") "    "
                               ((melting_point units "Kelvin") "1090") "    "
                               (symbol "As") "    "
                               ((density units "grams/cubic centimeter")
                                "      5.78    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s2 p3 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.2")
                               "    " (electronegativity "2.18") "    "
                               ((atomic_radius units "Angstroms") "1.39")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      32.4    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      13.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      27.7    ")
                               "    " (ionization_potential "9.81") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.33    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          50    ")
                               "  ")
                         "  "
                         (atom "    " (name "Astatine") "    "
                               (atomic_weight "210") "    "
                               (atomic_number "85") "    "
                               (oxidation_states "+/-1, 3, 5, 7") "    "
                               ((boiling_point units "Kelvin") "610") "    "
                               ((melting_point units "Kelvin") "575") "    "
                               (symbol "At") "    "
                               (electron_configuration
                                "[Xe] 4f14 5d10 6s2 p5 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.47")
                               "    " (electronegativity "2.2") "    "
                               ((atomic_radius units "Angstroms") "1.45")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      30    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      12    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          1.7    ")
                               "  ")
                         "  "
                         ((atom state "SOLID") "    " (name "Gold") "    "
                          (atomic_weight "196.9665") "    "
                          (atomic_number "79") "    "
                          (oxidation_states "3, 1") "    "
                          ((boiling_point units "Kelvin") "3130") "    "
                          ((melting_point units "Kelvin") "1337.58") "    "
                          (symbol "Au") "    "
                          ((density units "grams/cubic centimeter")
                           "      19.3    ")
                          "    "
                          (electron_configuration "[Xe] 4f14 5d10 6s1 ")
                          "    " ((covalent_radius units "Angstroms") "1.34")
                          "    " (electronegativity "2.54") "    "
                          ((atomic_radius units "Angstroms") "1.46") "    "
                          ((heat_of_vaporization units "kilojoules/mole")
                           "      324.43    ")
                          "    "
                          ((atomic_volume units "cubic centimeters/mole")
                           "      10.2    ")
                          "    "
                          ((heat_of_fusion units "kilojoules/mole")
                           "      12.36    ")
                          "    " (ionization_potential "9.225") "    "
                          ((specific_heat_capacity units
                            "Joules/gram/degree Kelvin")
                           "      0.128    ")
                          "    "
                          ((thermal_conductivity units
                            "Watts/meter/degree Kelvin")
                           "          317    ")
                          "  ")
                         "  "
                         (atom "    " (name "Boron") "    "
                               (atomic_weight "10.811") "    "
                               (atomic_number "5") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "4275") "    "
                               ((melting_point units "Kelvin") "2365") "    "
                               (symbol "B") "    "
                               ((density units "grams/cubic centimeter")
                                "      2.34    ")
                               "    " (electron_configuration "1s2 2s2 p1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "0.82")
                               "    " (electronegativity "2.04") "    "
                               ((atomic_radius units "Angstroms") "0.98")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      507.8    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      4.6    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      22.6    ")
                               "    " (ionization_potential "8.298") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      1.026    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          27    ")
                               "  ")
                         "  "
                         (atom "    " (name "Barium") "    "
                               (atomic_weight "137.33") "    "
                               (atomic_number "56") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "2078") "    "
                               ((melting_point units "Kelvin") "1002") "    "
                               (symbol "Ba") "    "
                               ((density units "grams/cubic centimeter")
                                "      3.59    ")
                               "    " (electron_configuration "[Xe] 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.98")
                               "    " (electronegativity "0.89") "    "
                               ((atomic_radius units "Angstroms") "2.22")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      140.2    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      39    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      8.01    ")
                               "    " (ionization_potential "5.212") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.204    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          18.4    ")
                               "  ")
                         "  "
                         (atom "    " (name "Beryllium") "    "
                               (atomic_weight "9.01218") "    "
                               (atomic_number "4") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "3243") "    "
                               ((melting_point units "Kelvin") "1560") "    "
                               (symbol "Be") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.85    ")
                               "    " (electron_configuration "1s2 2s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "0.9")
                               "    " (electronegativity "1.57") "    "
                               ((atomic_radius units "Angstroms") "1.12")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      297    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      11.71    ")
                               "    " (ionization_potential "9.322") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      1.825    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          200    ")
                               "  ")
                         "  "
                         (atom "    " (name "Bohrium") "    "
                               (atomic_weight "262") "    "
                               (atomic_number "107") "    " (symbol "Bh")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d5 7s2 ")
                               "  ")
                         "  "
                         ((atom state "SOLID") "    " (name "Bismuth") "    "
                          (atomic_weight "208.9804") "    "
                          (atomic_number "83") "    "
                          (oxidation_states "3, 5") "    "
                          ((boiling_point units "Kelvin") "1837") "    "
                          ((melting_point units "Kelvin") "544.59") "    "
                          (symbol "Bi") "    "
                          ((density units "grams/cubic centimeter")
                           "      9.75    ")
                          "    "
                          (electron_configuration "[Xe] 4f14 5d10 6s2 p3 ")
                          "    " ((covalent_radius units "Angstroms") "1.46")
                          "    " (electronegativity "2.02") "    "
                          ((atomic_radius units "Angstroms") "1.7") "    "
                          ((heat_of_vaporization units "kilojoules/mole")
                           "      179    ")
                          "    "
                          ((atomic_volume units "cubic centimeters/mole")
                           "      21.3    ")
                          "    "
                          ((heat_of_fusion units "kilojoules/mole")
                           "      11    ")
                          "    " (ionization_potential "7.289") "    "
                          ((specific_heat_capacity units
                            "Joules/gram/degree Kelvin")
                           "      0.122    ")
                          "    "
                          ((thermal_conductivity units
                            "Watts/meter/degree Kelvin")
                           "          7.87    ")
                          "  ")
                         "  "
                         (atom "    " (name "Berkelium") "    "
                               (atomic_weight "247") "    "
                               (atomic_number "97") "    "
                               (oxidation_states "4, 3") "    " (symbol "Bk")
                               "    " (electron_configuration "[Rn] 5f9 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               (ionization_potential "6.23") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Bromine") "    "
                               (atomic_weight "79.904") "    "
                               (atomic_number "35") "    "
                               (oxidation_states "+/-1, 5") "    "
                               ((boiling_point units "Kelvin") "331.85")
                               "    "
                               ((melting_point units "Kelvin") "265.95")
                               "    " (symbol "Br") "    "
                               ((density units "grams/cubic centimeter")
                                "      3.12    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s2 p5 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.14")
                               "    " (electronegativity "2.96") "    "
                               ((atomic_radius units "Angstroms") "1.12")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      14.725    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      23.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      5.286    ")
                               "    " (ionization_potential "11.814") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.226    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.122    ")
                               "  ")
                         "  "
                         ((atom state "SOLID") "    " (name "Carbon") "    "
                          (atomic_weight "12.011") "    " (atomic_number "6")
                          "    " (oxidation_states "+/-4, 2") "    "
                          ((boiling_point units "Kelvin") "5100") "    "
                          ((melting_point units "Kelvin") "3825") "    "
                          (symbol "C") "    "
                          ((density units "grams/cubic centimeter")
                           "      2.26    ")
                          "    " (electron_configuration "1s2 2s2 p2 ") "    "
                          ((covalent_radius units "Angstroms") "0.77") "    "
                          (electronegativity "2.55") "    "
                          ((atomic_radius units "Angstroms") "0.91") "    "
                          ((heat_of_vaporization units "kilojoules/mole")
                           "      715    ")
                          "    "
                          ((atomic_volume units "cubic centimeters/mole")
                           "      5.3    ")
                          "    " (ionization_potential "11.26") "    "
                          ((specific_heat_capacity units
                            "Joules/gram/degree Kelvin")
                           "      0.709    ")
                          "    "
                          ((thermal_conductivity units
                            "Watts/meter/degree Kelvin")
                           "          155    ")
                          "  ")
                         "  "
                         (atom "    " (name "Calcium") "    "
                               (atomic_weight "40.078") "    "
                               (atomic_number "20") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "1757") "    "
                               ((melting_point units "Kelvin") "1112") "    "
                               (symbol "Ca") "    "
                               ((density units "grams/cubic centimeter") "!
      1.55    ")
                               "    " (electron_configuration "[Ar] 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.74")
                               "    " (electronegativity "1") "    "
                               ((atomic_radius units "Angstroms") "1.97")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      154.67    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      29.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      8.53    ")
                               "    " (ionization_potential "6.113") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.647    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          200    ")
                               "  ")
                         "  "
                         (atom "    " (name "Cadmium") "    "
                               (atomic_weight "112.41") "    "
                               (atomic_number "48") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "1040") "    "
                               ((melting_point units "Kelvin") "594.26")
                               "    " (symbol "Cd") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.65    ")
                               "    "
                               (electron_configuration "[Kr] 4d10 5s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.41")
                               "    " (electronegativity "1.69") "    "
                               ((atomic_radius units "Angstroms") "1.71")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      99.87    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      13.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      6.07    ")
                               "    " (ionization_potential "8.993") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.233    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          96.8    ")
                               "  ")
                         "  "
                         (atom "    " (name "Cerium") "    "
                               (atomic_weight "140.12") "    "
                               (atomic_number "58") "    "
                               (oxidation_states "3, 4") "    "
                               ((boiling_point units "Kelvin") "3715") "    "
                               ((melting_point units "Kelvin") "1071") "    "
                               (symbol "Ce") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.77    ")
                               "    "
                               (electron_configuration "[Xe] 4f1 5d1 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.65")
                               "    " (electronegativity "1.12") "    "
                               ((atomic_radius units "Angstroms") "1.81")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      313.8    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      21    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      9.2    ")
                               "    " (ionization_potential "5.47") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.19    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          11.4    ")
                               "  ")
                         "  "
                         (atom "    " (name "Californium") "    "
                               (atomic_weight "251") "    "
                               (atomic_number "98") "    "
                               (oxidation_states "3") "    "
                               ((melting_point units "Kelvin") "1170") "    "
                               (symbol "Cf") "    "
                               (electron_configuration "[Rn] 5f10 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               (ionization_potential "6.3") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Chlorine") "    "
                               (atomic_weight "35.4527") "    "
                               (atomic_number "17") "    "
                               (oxidation_states "+/-1, 3, 5, 7") "    "
                               ((boiling_point units "Kelvin") "239.18")
                               "    "
                               ((melting_point units "Kelvin") "172.17")
                               "    " (symbol "Cl") "    "
                               ((density units "grams/cubic centimeter")
                                "      3.214    ")
                               "    "
                               (electron_configuration "[!
Ne] 3s2 p5 ")
                               "    "
                               ((covalent_radius units "Angstroms") "0.99")
                               "    " (electronegativity "3.16") "    "
                               ((atomic_radius units "Angstroms") "0.97")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      10.2    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      18.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      3.21    ")
                               "    " (ionization_potential "12.967") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.48    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.0089    ")
                               "  ")
                         "  "
                         (atom "    " (name "Curium") "    "
                               (atomic_weight "247") "    "
                               (atomic_number "96") "    "
                               (oxidation_states "3") "    "
                               ((melting_point units "Kelvin") "1620") "    "
                               (symbol "Cm") "    "
                               ((density units "grams/cubic centimeter")
                                "      13.5    ")
                               "    "
                               (electron_configuration "[Rn] 5f7 6d1 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      18.3    ")
                               "    " (ionization_potential "6.02") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Cobalt") "    "
                               (atomic_weight "58.9332") "    "
                               (atomic_number "27") "    "
                               (oxidation_states "2, 3") "    "
                               ((boiling_point units "Kelvin") "3143") "    "
                               ((melting_point units "Kelvin") "1768") "    "
                               (symbol "Co") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.9    ")
                               "    " (electron_configuration "[Ar] 3d7 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.16")
                               "    " (electronegativity "1.88") "    "
                               ((atomic_radius units "An! gstroms") "1.25")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      373.3    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      6.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      16.19    ")
                               "    " (ionization_potential "7.86") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.421    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          100    ")
                               "  ")
                         "  "
                         (atom "    " (name "Chromium") "    "
                               (atomic_weight "51.996") "    "
                               (atomic_number "24") "    "
                               (oxidation_states "6, 3, 2") "    "
                               ((boiling_point units "Kelvin") "2945") "    "
                               ((melting_point units "Kelvin") "2130") "    "
                               (symbol "Cr") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.19    ")
                               "    " (electron_configuration "[Ar] 3d5 4s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.18")
                               "    " (electronegativity "1.66") "    "
                               ((atomic_radius units "Angstroms") "1.3")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      339.5    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      7.23    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      20    ")
                               "    " (ionization_potential "6.766") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.449    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          93.7    ")
                               "  ")
                         "  "
                         (atom "    " (name "Cesium") "    "
                               (atomic_weight "132.9054") "    "
                               (atomic_number "55") "    "
                               (oxidation_states "1") "    "
                               ((boiling_point units "Kelvin") "944") "    "
                               ((melting_point units "Kelvin") "301.54")
                               "    " (symbol "Cs") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.87    ")
                               "    " (electron_configuration "[Xe] 6s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "2.35")
                               "    " (electronegativity "0.79") "    "
                               ((atomic_radius units "Angstroms") "2.67")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      67.74    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      70    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      2.092    ")
                               "    " (ionization_potential "3.894") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.24    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          35.9    ")
                               "  ")
                         "  "
                         (atom "    " (name "Copper") "    "
                               (atomic_weight "63.546") "    "
                               (atomic_number "29") "    "
                               (oxidation_states "2, 1") "    "
                               ((boiling_point units "Kelvin") "2840") "    "
                               ((melting_point units "Kelvin") "1356.6")
                               "    " (symbol "Cu") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.96    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.17")
                               "    " (electronegativity "1.9") "    "
                               ((atomic_radius units "Angstroms") "1.28")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      300.5    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      7.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      13.14    ")
                               "    " (ionization_potential "7.726") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.385    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          401    ")
                               "  ")
                         "  "
                         (atom "    " (name "Dubnium") "    "
                               (atomic_weight "262") "    "
                               (atomic_number "105") "    " (symbol "Db")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d3 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "Dysprosium") "    "
                               (atomic_weight "162.5") "    "
                               (atomic_number "66") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "2840") "    "
                               ((melting_point units "Kelvin") "1685") "    "
                               (symbol "Dy") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.55    ")
                               "    "
                               (electron_configuration "[Xe] 4f10 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.59")
                               "    " (electronegativity "1.22") "    "
                               ((atomic_radius units "Angstroms") "1.8")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      230    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      19    ")
                               "    " (ionization_potential "5.93") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.173    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10.7    ")
                               "  ")
                         "  "
                         (atom "    " (name "Erbium") "    "
                               (atomic_weight "167.26") "    "
                               (atomic_number "68") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3140") "    "
                               ((melting_point units "Kelvin") "1802") "    "
                               (symbol "Er") "    "
                               ((density units "grams/cubic centimeter")
                                "      9.07    ")
                               "    "
                               (electron_configuration "[Xe] 4f12 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.57")
                               "    " (electronegativity "1.24") "    "
                               ((atomic_radius units "Angstroms") "1.78")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      292.88    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      18.4    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      17.15    ")
                               "    " (ionization_potential "6.101") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.168    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          14.3    ")
                               "  ")
                         "  "
                         (atom "    " (name "Einsteinium") "    "
                               (atomic_weight "252") "    "
                               (atomic_number "99") "    "
                               ((melting_point units "Kelvin") "1130") "    "
                               (symbol "Es") "    "
                               (electron_configuration "[Rn] 5f11 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               (ionization_potential "6.42") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Europium") "    "
                               (atomic_weight "151.965") "    "
                               (atomic_number "63") "    "
                               (oxidation_states "3, 2") "    "
                               ((boiling_point units "Kelvin") "1800") "    "
                               ((melting_point units "Kelvin") "1095") "    "
                               (symbol "Eu") "    "
                               ((density units "grams/cubic centimeter")
                                "      5.24    ")
                               "    " (electron_configuration "[Xe] 4f7 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.85")
                               "    " (electronegativity "1.2") "    "
                               ((atomic_radius units "Angstroms") "1.99")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      175.73    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      28.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      9.21    ")
                               "    " (ionization_potential "5.67") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.182    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          13.9    ")
                               "  ")
                         "!
  "
                         (atom "    " (name "Fluorine") "    "
                               (atomic_weight "18.9984") "    "
                               (atomic_number "9") "    "
                               (oxidation_states "-1") "    "
                               ((boiling_point units "Kelvin") "85") "    "
                               ((melting_point units "Kelvin") "53.55") "    "
                               (symbol "F") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.696    ")
                               "    " (electron_configuration "1s2 2s2 p5 ")
                               "    "
                               ((covalent_radius units "Angstroms") "0.72")
                               "    " (electronegativity "3.98") "    "
                               ((atomic_radius units "Angstroms") "0.57")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      3.2698    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      17.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      0.26    ")
                               "    " (ionization_potential "17.422") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.824    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.0279    ")
                               "  ")
                         "  "
                         (atom "    " (name "Iron") "    "
                               (atomic_weight "55.847") "    "
                               (atomic_number "26") "    "
                               (oxidation_states "2, 3") "    "
                               ((boiling_point units "Kelvin") "3023") "    "
                               ((melting_point units "Kelvin") "1808") "    "
                               (symbol "Fe") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.874    ")
                               "    " (electron_configuration "[Ar] 3d6 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.17")
                               "    " (electronegativity "1.83") "    "
                               ((atomic_radius units "Angstroms") "1.26")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      349.5    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      7.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      13.8    ")
                               "    " (ionization_potential "7.87") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.449    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          80.2    ")
                               "  ")
                         "  "
                         (atom "    " (name "Fermium") "    "
                               (atomic_weight "257") "    "
                               (atomic_number "100") "    "
                               ((melting_point units "Kelvin") "1800") "    "
                               (symbol "Fm") "    "
                               (electron_configuration "[Rn] 5f12 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               (ionization_potential "6.5") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Francium") "    "
                               (atomic_weight "223") "    "
                               (atomic_number "87") "    "
                               (oxidation_states "1") "    "
                               ((boiling_point units "Kelvin") "950") "    "
                               ((melting_point units "Kelvin") "300") "    "
                               (symbol "Fr") "    "
                               (electron_configuration "[Rn] 7s1 ") "    "
                               (electronegativity "0.7") "    "
                               ((atomic_radius units "Angstroms") "2.7")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      64    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      2.1    ")
                               "    " (ionization_potential "0") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          15    ")
                               "  ")
                         "  "
                         (atom "    " (name "Gallium") "    "
                               (atomic_weight "69.723") "    "
                               (atomic_number "31") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "2478") "    "
                               ((melting_point units "Kelvin") "302.92")
                               "    " (symbol "Ga") "    "
                               ((density units "grams/cubic centimeter")
                                "      5.91    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s2 p1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.26")
                               "    " (electronegativity "1.81") "    "
                               ((atomic_radius units "Angstroms") "1.41")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      256.06    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      11.8    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      5.59    ")
                               "    " (ionization_potential "5.999") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.371    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          40.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "Gadolinium") "    "
                               (atomic_weight "157.25") "    "
                               (atomic_number "64") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3545") "    "
                               ((melting_point units "Kelvin") "1585") "    "
                               (symbol "Gd") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.9    ")
                               "    "
                               (electron_configuration "[Xe] 4f7 5d1 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.61")
                               "    " (electronegativity "1.2") "    "
                               ((atomic_radius units "Angstroms") "1.8")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      311.71    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      19.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      10.46    ")
                               "    " (ionization_potential "6.15") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.236    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "Germanium") "    "
                               (atomic_weight "72.61") "    "
                               (atomic_number "32") "    "
                               (oxidation_states "4") "    "
                               ((boiling_point units "Kelvin") "3107") "    "
                               ((melting_point units "Kelvin") "1211.5")
                               "    " (symbol "Ge") "    "
                               ((density units "grams/cubic centimeter")
                                "      5.32    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s2 p2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.22")
                               "    " (electronegativity "2.01") "    "
                               ((atomic_radius units "Angstroms") "1.37")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      334.3    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      13.6    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      31.8    ")
                               "    " (ionization_potential "7.899") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.32    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          59.9    ")
                               "  ")
                         "  "
                         ((atom state "GAS") "    " (name "Hydrogen") "    "
                          (atomic_weight "1.00794") "    " (atomic_number "1")
                          "    " (oxidation_states "1") "    "
                          ((boiling_point units "Kelvin") "20.28") "    "
                          ((melting_point units "Kelvin") "13.81") "    "
                          (symbol "H") "    "
                          ((density units "grams/cubic centimeter")
                           "      0.0899    ")
                          "    " (electron_configuration "1s1 ") "    "
                          ((covalent_radius units "Angstroms") "0.32") "    "
                          (electronegativity "2.1") "    "
                          ((atomic_radius units "Angstroms") "2.08") "    "
                          ((heat_of_vaporization units "kilojoules/mole")
                           "      0.4581    ")
                          "    "
                          ((atomic_volume units "cubic centimeters/mole")
                           "      14.1    ")
                          "    "
                          ((heat_of_fusion units "kilojoules/mole")
                           "      0.0585    ")
                          "    " (ionization_potential "13.598") "    "
                          ((specific_heat_capacity units
                            "Joules/gram/degree Kelvin")
                           "      14.304    ")
                          "    "
                          ((thermal_conductivity units
                            "Watts/meter/degree Kelvin")
                           "          0.1815    ")
                          "  ")
                         "  "
                         ((atom state "GAS") "    " (name "Helium") "    "
                          (atomic_weight "4.0026") "    " (atomic_number "2")
                          "    " ((boiling_point units "Kelvin") "4.216")
                          "    " ((melting_point units "Kelvin") "0.95")
                          "    " (symbol "He") "    "
                          ((density units "grams/cubic centimeter")
                           "      0.1785    ")
                          "    " (electron_configuration "1s2 ") "    "
                          ((covalent_radius units "Angstroms") "0.93") "    "
                          (electronegativity "0") "    "
                          ((heat_of_vaporization units "kilojoules/mole")
                           "      0.084    ")
                          "    "
                          ((atomic_volume units "cubic centimeters/mole")
                           "      31.8    ")
                          "    "
                          ((heat_of_fusion units "kilojoules/mole")
                           "      0.021    ")
                          "    " (ionization_potential "24.587") "    "
                          ((specific_heat_capacity units
                            "Joules/gram/degree Kelvin")
                           "      5.193    ")
                          "    "
                          ((thermal_conductivity units
                            "Watts/meter/degree Kelvin")
                           "          0.152    ")
                          "  ")
                         "  "
                         (atom "    " (name "Hafnium") "    "
                               (atomic_weight "178.49") "    "
                               (atomic_number "72") "    "
                               (oxidation_states "4") "    "
                               ((boiling_point units "Kelvin") "4875") "    "
                               ((melting_point units "Kelvin") "2504") "    "
                               (symbol "Hf") "    "
                               ((density units "grams/cubic centimeter")
                                "      13.31    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d2 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.44")
                               "    " (electronegativity "1.3") "    "
                               ((atomic_radius units "Angstroms") "1.67")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      661.07    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      13.6    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      21.76    ")
                               "    " (ionization_potential "6.65") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.14    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          23    ")
                               "  ")
                         "  "
                         (atom "    " (name "Mercury") "    "
                               (atomic_weight "200.59") "    "
                               (atomic_number "80") "    "
                               (oxidation_states "2, 1") "    "
                               ((boiling_point units "Kelvin") "629.88")
                               "    "
                               ((melting_point units "Kelvin") "234.31")
                               "    " (symbol "Hg") "    "
                               ((density units "grams/cubic centimeter")
                                "      13.55    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d10 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.49")
                               "    " (electronegativity "2") "    "
                               ((atomic_radius units "Angstroms") "1.6")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      59.3    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      14.8    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      2.292    ")
                               "    " (ionization_potential "10.437") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.140    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          8.34    ")
                               "  ")
                         "  "
                         (atom "    " (name "Holmium") "    "
                               (atomic_weight "164.9303") "    "
                               (atomic_number "67") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "2968") "    "
                               ((melting_point units "Kelvin") "1747") "    "
                               (symbol "Ho") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.8    ")
                               "    "
                               (electron_configuration "[Xe] 4f11 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.58")
                               "    " (electronegativity "1.23") "    "
                               ((atomic_radius units "Angstroms") "1.79")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      251.04    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      18.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      11.06    ")
                               "    " (ionization_potential "6.02") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.165    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          16.2    ")
                               "  ")
                         "  "
                         (atom "    " (name "Hassium") "    "
                               (atomic_weight "265") "    "
                               (atomic_number "108") "    " (symbol "Hs")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d6 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "Iodine") "    "
                               (atomic_weight "126.9045") "    "
                               (atomic_number "53") "    "
                               (oxidation_states "+/-1, 5, 7") "    "
                               ((boiling_point units "Kelvin") "457.5") "    "
                               ((melting_point units "Kelvin") "386.7") "    "
                               (symbol "I") "    "
                               ((density units "grams/cubic centimeter")
                                "      4.93    ")
                               "    "
                               (electron_configuration "[Kr] 4d10 5s2 p5 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.33")
                               "    " (electronegativity "2.66") "    "
                               ((atomic_radius units "Angstroms") "1.32")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      20.9    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      25.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      7.76    ")
                               "    " (ionization_potential "10.451") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.145    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.449    ")
                               "  ")
                         "  "
                         (atom "    " (name "Indium") "    "
                               (atomic_weight "114.82") "    "
                               (atomic_number "49") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "2350") "    "
                               ((melting_point units "Kelvin") "429.78")
                               "    " (symbol "In") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.31    ")
                               "    "
                               (electron_configuration "[Kr] 4d10 5s2 p1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.44")
                               "    " (electronegativity "1.78") "    "
                               ((atomic_radius units "Angstroms") "1.66")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      226.35    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      15.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      3.26    ")
                               "    " (ionization_potential "5.786") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.233    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          81.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "Iridium") "    "
                               (atomic_weight "192.22") "    "
                               (atomic_number "77") "    "
                               (oxidation_states "2, 3, 4, 6") "    "
                               ((boiling_point units "Kelvin") "4700") "    "
                               ((melting_point units "Kelvin") "2720") "    "
                               (symbol "Ir") "    "
                               ((density units "grams/cubic centimeter")
                                "      22.6    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d7 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.27")
                               "    " (electronegativity "2.2") "    "
                               ((atomic_radius units "Angstroms") "1.36")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      563.58    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.54    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      26.36    ")
                               "    " (ionization_potential "9.1") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.13    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          147    ")
                               "  ")
                         "  "
                         (atom "    " (name "Potassium") "    "
                               (atomic_weight "39.0983") "    "
                               (atomic_number "19") "    "
                               (oxidation_states "1") "    "
                               ((boiling_point units "Kelvin") "1033") "    "
                               ((melting_point units "Kelvin") "336.8") "    "
                               (symbol "K") "    "
                               ((density units "grams/cubic centimeter")
                                "      0.86    ")
                               "    " (electron_configuration "[Ar] 4s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "2.03")
                               "    " (electronegativity "0.82") "    "
                               ((atomic_radius units "Angstroms") "2.35")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      76.9    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      45.3    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      2.33    ")
                               "    " (ionization_potential "4.341") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.757    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          102.5    ")
                               "  ")
                         "  "
                         (atom "    " (name "Krypton") "    "
                               (atomic_weight "83.8") "    "
                               (atomic_number "36") "    "
                               ((boiling_point units "Kelvin") "120.85")
                               "    " ((melting_point units "Kelvin") "116")
                               "    " (symbol "Kr") "    "
                               ((density units "grams/cubic centimeter")
                                "      3.75    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s2 p6 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.89")
                               "    " (electronegativity "0") "    "
                               ((atomic_radius units "Angstroms") "1.03")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      9.029    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      32.2    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      1.638    ")
                               "    " (ionization_potential "13.999") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.248    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.00949    ")
                               "  ")
                         "  "
                         (atom "    " (name "Lanthanum") "    "
                               (atomic_weight "138.9055") "    "
                               (atomic_number "57") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3737") "    "
                               ((melting_point units "Kelvin") "1191") "    "
                               (symbol "La") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.15    ")
                               "    " (electron_configuration "[Xe] 5d1 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.25")
                               "    " (electronegativity "1.1") "    "
                               ((atomic_radius units "Angstroms") "1.38")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      399.57    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      22.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      11.3    ")
                               "    " (ionization_potential "5.58") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.19    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          13.5    ")
                               "  ")
                         "  "
                         (atom "    " (name "Lithium") "    "
                               (atomic_weight "6.941") "    "
                               (atomic_number "3") "    "
                               (oxidation_states "1") "    "
                               ((boiling_point units "Kelvin") "1615") "    "
                               ((melting_point units "Kelvin") "453.7") "    "
                               (symbol "Li") "    "
                               ((density units "grams/cubic centimeter")
                                "      0.53    ")
                               "    " (electron_configuration "1s2 2s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.23")
                               "    " (electronegativity "0.98") "    "
                               ((atomic_radius units "Angstroms") "1.55")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      147.1    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      13.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      3    ")
                               "    " (ionization_potential "5.392") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      3.582    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          84.7    ")
                               "  ")
                         "  "
                         (atom "    " (name "Lawrencium") "    "
                               (atomic_weight "262") "    "
                               (atomic_number "103") "    "
                               ((melting_point units "Kelvin") "1900") "    "
                               (symbol "Lr") "    "
                               (electron_configuration "[Rn] 5f14 6d1 7s2 ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Lutetium") "    "
                               (atomic_weight "174.967") "    "
                               (atomic_number "71") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3668") "    "
                               ((melting_point units "Kelvin") "1936") "    "
                               (symbol "Lu") "    "
                               ((density units "grams/cubic centimeter")
                                "      9.84    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d1 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.56")
                               "    " (electronegativity "1.27") "    "
                               ((atomic_radius units "Angstroms") "1.75")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      355    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      17.8    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      18.6    ")
                               "    " (ionization_potential "5.43") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.15    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          16.4    ")
                               "  ")
                         "  "
                         (atom "    " (name "Mendelevium") "    "
                               (atomic_weight "258") "    "
                               (atomic_number "101") "    "
                               ((melting_point units "Kelvin") "1100") "    "
                               (symbol "Md") "    "
                               (electron_configuration "[Rn] 5f13 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               (ionization_potential "6.58") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Magnesium") "    "
                               (atomic_weight "24.305") "    "
                               (atomic_number "12") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "1380") "    "
                               ((melting_point units "Kelvin") "922") "    "
                               (symbol "Mg") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.74    ")
                               "    " (electron_configuration "[Ne] 3s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.36")
                               "    " (electronegativity "1.31") "    "
                               ((atomic_radius units "Angstroms") "1.6")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      127.6    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      14    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      8.95    ")
                               "    " (ionization_potential "7.646") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      1.02    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          156    ")
                               "  ")
                         "  "
                         (atom "    " (name "Manganese") "    "
                               (atomic_weight "54.938") "    "
                               (atomic_number "25") "    "
                               (oxidation_states "7, 6, 4, 2, 3") "    "
                               ((boiling_point units "Kelvin") "2335") "    "
                               ((melting_point units "Kelvin") "1518") "    "
                               (symbol "Mn") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.44    ")
                               "    " (electron_configuration "[Ar] 3d5 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.17")
                               "    " (electronegativity "1.55") "    "
                               ((atomic_radius units "Angstroms") "1.35")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      219.74    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      7.39    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      14.64    ")
                               "    " (ionization_potential "7.435") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.48    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          7.82    ")
                               "  ")
                         "  "
                         (atom "    " (name "Molybdenum") "    "
                               (atomic_weight "95.94") "    "
                               (atomic_number "42") "    "
                               (oxidation_states "6, 5, 4, 3, 2") "    "
                               ((boiling_point units "Kelvin") "4912") "    "
                               ((melting_point units "Kelvin") "2896") "    "
                               (symbol "Mo") "    "
                               ((density units "grams/cubic centimeter")
                                "      10.22    ")
                               "    " (electron_configuration "[Kr] 4d5 5s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.3")
                               "    " (electronegativity "2.16") "    "
                               ((atomic_radius units "Angstroms") "1.39")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      590.4    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      9.4    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      36    ")
                               "    " (ionization_potential "7.099") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.25    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          138    ")
                               "  ")
                         "  "
                         (atom "    " (name "Meitnerium") "    "
                               (atomic_weight "266") "    "
                               (atomic_number "109") "    " (symbol "Mt")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d7 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "Nitrogen") "    "
                               (atomic_weight "14.0067") "    "
                               (atomic_number "7") "    "
                               (oxidation_states "+/-3, 5, 4, 2") "    "
                               ((boiling_point units "Kelvin") "77.344")
                               "    " ((melting_point units "Kelvin") "63.15")
                               "    " (symbol "N") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.251    ")
                               "    " (electron_configuration "1s2 2s2 p3 ")
                               "    "
                               ((covalent_radius units "Angstroms") "0.75")
                               "    " (electronegativity "3.04") "    "
                               ((atomic_radius units "Angstroms") "0.92")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      2.7928    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      17.3    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      0.36    ")
                               "    " (ionization_potential "14.534") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      1.042    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.02598    ")
                               "  ")
                         "  "
                         (atom "    " (name "Sodium") "    "
                               (atomic_weight "22.98977") "    "
                               (atomic_number "11") "    "
                               (oxidation_states "1") "    "
                               ((boiling_point units "Kelvin") "1156") "    "
                               ((melting_point units "Kelvin") "371") "    "
                               (symbol "Na") "    "
                               ((density units "grams/cubic centimeter")
                                "      0.97    ")
                               "    " (electron_configuration "[Ne] 3s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.54")
                               "    " (electronegativity "0.93") "    "
                               ((atomic_radius units "Angstroms") "1.9")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      98.01    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      23.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      2.601    ")
                               "    " (ionization_potential "5.139") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      1.23    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          141    ")
                               "  ")
                         "  "
                         (atom "    " (name "Niobium") "    "
                               (atomic_weight "92.9064") "    "
                               (atomic_number "41") "    "
                               (oxidation_states "5, 3") "    "
                               ((boiling_point units "Kelvin") "5015") "    "
                               ((melting_point units "Kelvin") "2742") "    "
                               (symbol "Nb") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.57    ")
                               "    " (electron_configuration "[Kr] 4d4 5s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.34")
                               "    " (electronegativity "1.6") "    "
                               ((atomic_radius units "Angstroms") "1.46")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      690.1    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      10.8    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      26.9    ")
                               "    " (ionization_potential "6.88") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.265    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          53.7    ")
                               "  ")
                         "  "
                         (atom "    " (name "Neodymium") "    "
                               (atomic_weight "144.24") "    "
                               (atomic_number "60") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3347") "    "
                               ((melting_point units "Kelvin") "1294") "    "
                               (symbol "Nd") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.01    ")
                               "    " (electron_configuration "[Xe] 4f4 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.64")
                               "    " (electronegativity "1.14") "    "
                               ((atomic_radius units "Angstroms") "1.82")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      283.68    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      20.6    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      10.88    ")
                               "    " (ionization_potential "5.49") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.19    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          16.5    ")
                               "  ")
                         "  "
                         (atom "    " (name "Neon") "    "
                               (atomic_weight "20.1797") "    "
                               (atomic_number "10") "    "
                               ((boiling_point units "Kelvin") "27.1") "    "
                               ((melting_point units "Kelvin") "24.55") "    "
                               (symbol "Ne") "    "
                               ((density units "grams/cubic centimeter")
                                "      0.900    ")
                               "    " (electron_configuration "1s2 2s2 p6 ")
                               "    "
                               ((covalent_radius units "Angstroms") "0.71")
                               "    " (electronegativity "0") "    "
                               ((atomic_radius units "Angstroms") "0.51")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      1.77    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      16.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      0.34    ")
                               "    " (ionization_potential "21.564") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      1.03    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.0493    ")
                               "  ")
                         "  "
                         (atom "    " (name "Nickel") "    "
                               (atomic_weight "58.6934") "    "
                               (atomic_number "28") "    "
                               (oxidation_states "2, 3") "    "
                               ((boiling_point units "Kelvin") "3005") "    "
                               ((melting_point units "Kelvin") "1726") "    "
                               (symbol "Ni") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.9    ")
                               "    " (electron_configuration "[Ar] 3d8 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.15")
                               "    " (electronegativity "1.91") "    "
                               ((atomic_radius units "Angstroms") "1.24")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      377.5    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      6.6    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      17.2    ")
                               "    " (ionization_potential "7.635") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.444    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          90.7    ")
                               "  ")
                         "  "
                         (atom "    " (name "Nobelium") "    "
                               (atomic_weight "259") "    "
                               (atomic_number "102") "    "
                               ((melting_point units "Kelvin") "1100") "    "
                               (symbol "No") "    "
                               (electron_configuration "[Rn] 5f14 7s2 ")
                               "    " (electronegativity "1.3") "    "
                               (ionization_potential "6.65") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          10    ")
                               "  ")
                         "  "
                         (atom "    " (name "Neptunium") "    "
                               (atomic_weight "237.0482") "    "
                               (atomic_number "93") "    "
                               (oxidation_states "6, 5, 4, 3") "    "
                               ((boiling_point units "Kelvin") "4175") "    "
                               ((melting_point units "Kelvin") "912") "    "
                               (symbol "Np") "    "
                               ((density units "grams/cubic centimeter")
                                "      20.2    ")
                               "    "
                               (electron_configuration "[Rn] 5f4 6d1 7s2 ")
                               "    " (electronegativity "1.36") "    "
                               ((atomic_radius units "Angstroms") "1.3")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      21.1    ")
                               "    " (ionization_potential "6.19") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          6.3    ")
                               "  ")
                         "  "
                         (atom "    " (name "Oxygen") "    "
                               (atomic_weight "15.9994") "    "
                               (atomic_number "8") "    "
                               (oxidation_states "-2") "    "
                               ((boiling_point units "Kelvin") "90.188")
                               "    " ((melting_point units "Kelvin") "54.8")
                               "    " (symbol "O") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.429    ")
                               "    " (electron_configuration "1s2 2s2 p4 ")
                               "    "
                               ((covalent_radius units "Angstroms") "0.73")
                               "    " (electronegativity "3.44") "    "
                               ((atomic_radius units "Angstroms") "0.65")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      3.4109    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      14    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      0.222    ")
                               "    " (ionization_potential "13.618") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.92    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.2674    ")
                               "  ")
                         "  "
                         (atom "    " (name "Osmium") "    "
                               (atomic_weight "190.2") "    "
                               (atomic_number "76") "    "
                               (oxidation_states "2, 3, 4, 6, 8") "    "
                               ((boiling_point units "Kelvin") "5300") "    "
                               ((melting_point units "Kelvin") "3300") "    "
                               (symbol "Os") "    "
                               ((density units "grams/cubic centimeter")
                                "      22.6    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d6 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.26")
                               "    " (electronegativity "2.2") "    "
                               ((atomic_radius units "Angstroms") "1.35")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      627.6    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.43    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      29.29    ")
                               "    " (ionization_potential "8.7") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.13    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          87.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "Phosphorus") "    "
                               (atomic_weight "30.97376") "    "
                               (atomic_number "15") "    "
                               (oxidation_states "+/-3, 5, 4") "    "
                               ((boiling_point units "Kelvin") "553") "    "
                               ((melting_point units "Kelvin") "317.3") "    "
                               (symbol "P") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.82    ")
                               "    " (electron_configuration "[Ne] 3s2 p3 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.06")
                               "    " (electronegativity "2.19") "    "
                               ((atomic_radius units "Angstroms") "1.28")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      12.4    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      17    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      0.63    ")
                               "    " (ionization_potential "10.486") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.769    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.235    ")
                               "  ")
                         "  "
                         (atom "    " (name "Protactinium") "    "
                               (atomic_weight "231.0359") "    "
                               (atomic_number "91") "    "
                               (oxidation_states "5, 4") "    "
                               ((boiling_point units "Kelvin") "4300") "    "
                               ((melting_point units "Kelvin") "1845") "    "
                               (symbol "Pa") "    "
                               ((density units "grams/cubic centimeter")
                                "      15.4    ")
                               "    "
                               (electron_configuration "[Rn] 5f2 6d1 7s2 ")
                               "    " (electronegativity "1.5") "    "
                               ((atomic_radius units "Angstroms") "1.61")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      15    ")
                               "    " (ionization_potential "5.88") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          47    ")
                               "  ")
                         "  "
                         (atom "    " (name "Lead") "    "
                               (atomic_weight "207.2") "    "
                               (atomic_number "82") "    "
                               (oxidation_states "4, 2") "    "
                               ((boiling_point units "Kelvin") "2023") "    "
                               ((melting_point units "Kelvin") "600.65")
                               "    " (symbol "Pb") "    "
                               ((density units "grams/cubic centimeter")
                                "      11.35    ")
                               "    "
                               (electron_configuration
                                "[Xe] 4f14 5d10 6s2 p2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.47")
                               "    " (electronegativity "2.33") "    "
                               ((atomic_radius units "Angstroms") "1.75")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      177.9    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      18.3    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      4.77    ")
                               "    " (ionization_potential "7.416") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.129    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          35.3    ")
                               "  ")
                         "  "
                         (atom "    " (name "Palladium") "    "
                               (atomic_weight "106.42") "    "
                               (atomic_number "46") "    "
                               (oxidation_states "2, 4") "    "
                               ((boiling_point units "Kelvin") "3240") "    "
                               ((melting_point units "Kelvin") "1825") "    "
                               (symbol "Pd") "    "
                               ((density units "grams/cubic centimeter")
                                "      12    ")
                               "    " (electron_configuration "[Kr] 4d10 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.28")
                               "    " (electronegativity "2.2") "    "
                               ((atomic_radius units "Angstroms") "1.37")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      393.3    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      16.74    ")
                               "    " (ionization_potential "8.34") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.244    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          71.8    ")
                               "  ")
                         "  "
                         (atom "    " (name "Promethium") "    "
                               (atomic_weight "145") "    "
                               (atomic_number "61") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3273") "    "
                               ((melting_point units "Kelvin") "1315") "    "
                               (symbol "Pm") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.22    ")
                               "    " (electron_configuration "[Xe] 4f5 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.63")
                               "    " (electronegativity "1.13") "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      22.4    ")
                               "    " (ionization_potential "5.55") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          17.9    ")
                               "  ")
                         "  "
                         (atom "    " (name "Polonium") "    "
                               (atomic_weight "209") "    "
                               (atomic_number "84") "    "
                               (oxidation_states "4, 2") "    "
                               ((melting_point units "Kelvin") "527") "    "
                               (symbol "Po") "    "
                               ((density units "grams/cubic centimeter")
                                "      9.3    ")
                               "    "
                               (electron_configuration
                                "[Xe] 4f14 5d10 6s2 p4 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.53")
                               "    " (electronegativity "2") "    "
                               ((atomic_radius units "Angstroms") "1.67")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      120    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      22.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      13    ")
                               "    " (ionization_potential "8.42") "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          20    ")
                               "  ")
                         "  "
                         (atom "    " (name "Praseodymium") "    "
                               (atomic_weight "140.9077") "    "
                               (atomic_number "59") "    "
                               (oxidation_states "3, 4") "    "
                               ((boiling_point units "Kelvin") "3785") "    "
                               ((melting_point units "Kelvin") "1204") "    "
                               (symbol "Pr") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.77    ")
                               "    " (electron_configuration "[Xe] 4f3 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.65")
                               "    " (electronegativity "1.13") "    "
                               ((atomic_radius units "Angstroms") "1.82")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      332.63    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      20.8    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      10.04    ")
                               "    " (ionization_potential "5.42") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.193    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          12.5    ")
                               "  ")
                         "  "
                         (atom "    " (name "Platinum") "    "
                               (atomic_weight "195.08") "    "
                               (atomic_number "78") "    "
                               (oxidation_states "2, 4") "    "
                               ((boiling_point units "Kelvin") "4100") "    "
                               ((melting_point units "Kelvin") "2042.1")
                               "    " (symbol "Pt") "    "
                               ((density units "grams/cubic centimeter")
                                "      21.45    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d9 6s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.3")
                               "    " (electronegativity "2.28") "    "
                               ((atomic_radius units "Angstroms") "1.39")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      510.45    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      9.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      19.66    ")
                               "    " (ionization_potential "9") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.13    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          71.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "Plutonium") "    "
                               (atomic_weight "244") "    "
                               (atomic_number "94") "    "
                               (oxidation_states "6, 5, 4, 3") "    "
                               ((boiling_point units "Kelvin") "3505") "    "
                               ((melting_point units "Kelvin") "913") "    "
                               (symbol "Pu") "    "
                               ((density units "grams/cubic centimeter")
                                "      19.84    ")
                               "    " (electron_configuration "[Rn] 5f6 7s2 ")
                               "    " (electronegativity "1.28") "    "
                               ((atomic_radius units "Angstroms") "1.51")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      12.32    ")
                               "    " (ionization_potential "6.06") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.13    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          6.74    ")
                               "  ")
                         "  "
                         (atom "    " (name "Radium") "    "
                               (atomic_weight "226.0254") "    "
                               (atomic_number "88") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "1413") "    "
                               ((melting_point units "Kelvin") "973") "    "
                               (symbol "Ra") "    "
                               ((density units "grams/cubic centimeter")
                                "      5    ")
                               "    " (electron_configuration "[Rn] 7s2 ")
                               "    " (electronegativity "0.89") "    "
                               ((atomic_radius units "Angstroms") "2.33")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      136.82    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      45.2    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      8.37    ")
                               "    " (ionization_potential "5.279") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.094    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          18.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "Rubidium") "    "
                               (atomic_weight "85.4678") "    "
                               (atomic_number "37") "    "
                               (oxidation_states "1") "    "
                               ((boiling_point units "Kelvin") "961") "    "
                               ((melting_point units "Kelvin") "312.63")
                               "    " (symbol "Rb") "    "
                               ((density units "grams/cubic centimeter")
                                "      1.532    ")
                               "    " (electron_configuration "[Kr] 5s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "2.16")
                               "    " (electronegativity "0.82") "    "
                               ((atomic_radius units "Angstroms") "2.48")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      69.2    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      55.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      2.34    ")
                               "    " (ionization_potential "4.177") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.363    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          58.2    ")
                               "  ")
                         "  "
                         (atom "    " (name "Rhenium") "    "
                               (atomic_weight "186.207") "    "
                               (atomic_number "75") "    "
                               (oxidation_states "7, 6, 4, 2, -1") "    "
                               ((boiling_point units "Kelvin") "5870") "    "
                               ((melting_point units "Kelvin") "3455") "    "
                               (symbol "Re") "    "
                               ((density units "grams/cubic centimeter")
                                "      21    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d5 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.28")
                               "    " (electronegativity "1.9") "    "
                               ((atomic_radius units "Angstroms") "1.37")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      707.1    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.85    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      33.05    ")
                               "    " (ionization_potential "7.88") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.137    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          47.9    ")
                               "  ")
                         "  "
                         (atom "    " (name "Rutherfordium") "    "
                               (atomic_weight "261") "    "
                               (atomic_number "104") "    " (symbol "Rf")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d2 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "Rhodium") "    "
                               (atomic_weight "102.9055") "    "
                               (atomic_number "45") "    "
                               (oxidation_states "2, 3, 4") "    "
                               ((boiling_point units "Kelvin") "3970") "    "
                               ((melting_point units "Kelvin") "2236") "    "
                               (symbol "Rh") "    "
                               ((density units "grams/cubic centimeter")
                                "      12.41    ")
                               "    " (electron_configuration "[Kr] 4d8 5s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.25")
                               "    " (electronegativity "2.28") "    "
                               ((atomic_radius units "Angstroms") "1.34")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      495.39    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.3    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      21.76    ")
                               "    " (ionization_potential "7.46") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.242    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          150    ")
                               "  ")
                         "  "
                         (atom "    " (name "Radon") "    "
                               (atomic_weight "222") "    "
                               (atomic_number "86") "    "
                               ((boiling_point units "Kelvin") "211.4") "    "
                               ((melting_point units "Kelvin") "202") "    "
                               (symbol "Rn") "    "
                               ((density units "grams/cubic centimeter")
                                "      9.73    ")
                               "    "
                               (electron_configuration
                                "[Xe] 4f14 5d10 6s2 p6 ")
                               "    " (electronegativity "0") "    "
                               ((atomic_radius units "Angstroms") "1.34")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      16.4    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      50.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      2.9    ")
                               "    " (ionization_potential "10.748") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.094    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.00364    ")
                               "  ")
                         "  "
                         (atom "    " (name "Ruthenium") "    "
                               (atomic_weight "101.07") "    "
                               (atomic_number "44") "    "
                               (oxidation_states "2, 3, 4, 6, 8") "    "
                               ((boiling_point units "Kelvin") "4425") "    "
                               ((melting_point units "Kelvin") "2610") "    "
                               (symbol "Ru") "    "
                               ((density units "grams/cubic centimeter")
                                "      12.37    ")
                               "    " (electron_configuration "[Kr] 4d7 5s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.25")
                               "    " (electronegativity "2.2") "    "
                               ((atomic_radius units "Angstroms") "1.34")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      567.77    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.3    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      25.52    ")
                               "    " (ionization_potential "7.37") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.238    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          117    ")
                               "  ")
                         "  "
                         (atom "    " (name "Sulfur") "    "
                               (atomic_weight "32.066") "    "
                               (atomic_number "16") "    "
                               (oxidation_states "+/-2, 4, 6") "    "
                               ((boiling_point units "Kelvin") "717.82")
                               "    " ((melting_point units "Kelvin") "392.2")
                               "    " (symbol "S") "    "
                               ((density units "grams/cubic centimeter")
                                "      2.07    ")
                               "    " (electron_configuration "[Ne] 3s2 p4 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.02")
                               "    " (electronegativity "2.58") "    "
                               ((atomic_radius units "Angstroms") "1.27")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      10    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      15.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      1.73    ")
                               "    " (ionization_potential "10.36") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.71    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          0.269    ")
                               "  ")
                         "  "
                         (atom "    " (name "Silver") "    "
                               (atomic_weight "107.868") "    "
                               (atomic_number "47") "    "
                               (oxidation_states "1") "    "
                               ((boiling_point units "Kelvin") "2436") "    "
                               ((melting_point units "Kelvin") "1235.08")
                               "    " (symbol "Ag") "    "
                               ((density units "grams/cubic centimeter")
                                "      10.5    ")
                               "    "
                               (electron_configuration "[Kr] 4d10 5s1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.34")
                               "    " (electronegativity "1.93") "    "
                               ((atomic_radius units "Angstroms") "1.44")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      250.63    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      10.3    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      11.3    ")
                               "    " (ionization_potential "7.576") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.232    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          429    ")
                               "  ")
                         "    "
                         (atom "    " (name "Scandium") "    "
                               (atomic_weight "44.9559") "    "
                               (atomic_number "21") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3109") "    "
                               ((melting_point units "Kelvin") "1814") "    "
                               (symbol "Sc") "    "
                               ((density units "grams/cubic centimeter")
                                "      2.99    ")
                               "    " (electron_configuration "[Ar] 3d1 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.44")
                               "    " (electronegativity "1.36") "    "
                               ((atomic_radius units "Angstroms") "1.62")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      304.8    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      15    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      16.11    ")
                               "    " (ionization_potential "6.54") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.568    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          15.8    ")
                               "  ")
                         "  "
                         (atom "    " (name "Selenium") "    "
                               (atomic_weight "78.96") "    "
                               (atomic_number "34") "    "
                               (oxidation_states "-2, 4, 6") "    "
                               ((boiling_point units "Kelvin") "958") "    "
                               ((melting_point units "Kelvin") "494") "    "
                               (symbol "Se") "    "
                               ((density units "grams/cubic centimeter")
                                "      4.79    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s2 p4 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.16")
                               "    " (electronegativity "2.55") "    "
                               ((atomic_radius units "Angstroms") "1.4")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      26.32    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      16.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      5.54    ")
                               "    " (ionization_potential "9.752") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.32    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          2.04    ")
                               "  ")
                         "  "
                         (atom "    " (name "Seaborgium") "    "
                               (atomic_weight "263") "    "
                               (atomic_number "106") "    " (symbol "Sg")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d4 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "Silicon") "    "
                               (atomic_weight "28.0855") "    "
                               (atomic_number "14") "    "
                               (oxidation_states "4") "    "
                               ((boiling_point units "Kelvin") "2630") "    "
                               ((melting_point units "Kelvin") "1683") "    "
                               (symbol "Si") "    "
                               ((density units "grams/cubic centimeter")
                                "      2.33    ")
                               "    " (electron_configuration "[Ne] 3s2 p2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.11")
                               "    " (electronegativity "1.9") "    "
                               ((atomic_radius units "Angstroms") "1.32")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      359    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      12.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      50.2    ")
                               "    " (ionization_potential "8.151") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.70    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          148    ")
                               "  ")
                         "  "
                         (atom "    " (name "Samarium") "    "
                               (atomic_weight "150.36") "    "
                               (atomic_number "62") "    "
                               (oxidation_states "3, 2") "    "
                               ((boiling_point units "Kelvin") "2067") "    "
                               ((melting_point units "Kelvin") "1347") "    "
                               (symbol "Sm") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.52    ")
                               "    " (electron_configuration "[Xe] 4f6 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.62")
                               "    " (electronegativity "1.17") "    "
                               ((atomic_radius units "Angstroms") "1.81")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      191.63    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      19.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      11.09    ")
                               "    " (ionization_potential "5.63") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.197    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          13.3    ")
                               "  ")
                         "  "
                         (atom "    " (name "Strontium") "    "
                               (atomic_weight "87.62") "    "
                               (atomic_number "38") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "1655") "    "
                               ((melting_point units "Kelvin") "1042") "    "
                               (symbol "Sr") "    "
                               ((density units "grams/cubic centimeter")
                                "      2.54    ")
                               "    " (electron_configuration "[Kr] 5s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.91")
                               "    " (electronegativity "0.95") "    "
                               ((atomic_radius units "Angstroms") "2.15")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      136.9    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      33.7    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      8.2    ")
                               "    " (ionization_potential "5.695") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.3    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          35.3    ")
                               "  ")
                         "  "
                         (atom "    " (name "Tantalum") "    "
                               (atomic_weight "180.9479") "    "
                               (atomic_number "73") "    "
                               (oxidation_states "5") "    "
                               ((boiling_point units "Kelvin") "5730") "    "
                               ((melting_point units "Kelvin") "3293") "    "
                               (symbol "Ta") "    "
                               ((density units "grams/cubic centimeter")
                                "      16.65    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d3 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.34")
                               "    " (electronegativity "1.5") "    "
                               ((atomic_radius units "Angstroms") "1.49")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      737    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      10.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      36    ")
                               "    " (ionization_potential "7.89") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.14    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          57.5    ")
                               "  ")
                         "  "
                         (atom "    " (name "Terbium") "    "
                               (atomic_weight "158.9253") "    "
                               (atomic_number "65") "    "
                               (oxidation_states "3, 4") "    "
                               ((boiling_point units "Kelvin") "3500") "    "
                               ((melting_point units "Kelvin") "1629") "    "
                               (symbol "Tb") "    "
                               ((density units "grams/cubic centimeter")
                                "      8.23    ")
                               "    " (electron_configuration "[Xe] 4f9 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.59")
                               "    " (electronegativity "1.1") "    "
                               ((atomic_radius units "Angstroms") "1.8")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      19.2    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      15.48    ")
                               "    " (ionization_potential "5.86") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.18    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          11.1    ")
                               "  ")
                         "  "
                         (atom "    " (name "Technetium") "    "
                               (atomic_weight "98") "    "
                               (atomic_number "43") "    "
                               (oxidation_states "7") "    "
                               ((boiling_point units "Kelvin") "4538") "    "
                               ((melting_point units "Kelvin") "2477") "    "
                               (symbol "Tc") "    "
                               ((density units "grams/cubic centimeter")
                                "      11.5    ")
                               "    " (electron_configuration "[Kr] 4d5 5s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.27")
                               "    " (electronegativity "1.9") "    "
                               ((atomic_radius units "Angstroms") "1.36")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      502    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      23    ")
                               "    " (ionization_potential "7.28") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.24    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          50.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "Tellurium") "    "
                               (atomic_weight "127.6") "    "
                               (atomic_number "52") "    "
                               (oxidation_states "-2, 4, 6") "    "
                               ((boiling_point units "Kelvin") "1261") "    "
                               ((melting_point units "Kelvin") "722.72")
                               "    " (symbol "Te") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.24    ")
                               "    "
                               (electron_configuration "[Kr] 4d10 5s2 p4 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.36")
                               "    " (electronegativity "2.1") "    "
                               ((atomic_radius units "Angstroms") "1.42")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      50.63    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      20.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      17.49    ")
                               "    " (ionization_potential "9.009") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.202    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          2.35    ")
                               "  ")
                         "  "
                         (atom "    " (name "Thorium") "    "
                               (atomic_weight "232.0381") "    "
                               (atomic_number "90") "    "
                               (oxidation_states "4") "    "
                               ((boiling_point units "Kelvin") "5060") "    "
                               ((melting_point units "Kelvin") "2028") "    "
                               (symbol "Th") "    "
                               ((density units "grams/cubic centimeter")
                                "      11.72    ")
                               "    " (electron_configuration "[Rn] 6d2 7s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.65")
                               "    " (electronegativity "1.3") "    "
                               ((atomic_radius units "Angstroms") "1.8")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      543.92    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      19.9    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      15.65    ")
                               "    " (ionization_potential "6.08") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.113    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          54    ")
                               "  ")
                         "  "
                         (atom "    " (name "Tin") "    "
                               (atomic_weight "118.71") "    "
                               (atomic_number "50") "    "
                               (oxidation_states "4, 2") "    "
                               ((boiling_point units "Kelvin") "2876") "    "
                               ((melting_point units "Kelvin") "505.12")
                               "    " (symbol "Sn") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.31    ")
                               "    "
                               (electron_configuration "[Kr] 4d10 5s2 p2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.41")
                               "    " (electronegativity "1.96") "    "
                               ((atomic_radius units "Angstroms") "1.62")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      290.37    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      16.3    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      7.2    ")
                               "    " (ionization_potential "7.344") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.228    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          66.6    ")
                               "  ")
                         "    "
                         (atom "    " (name "Titanium") "    "
                               (atomic_weight "47.88") "    "
                               (atomic_number "22") "    "
                               (oxidation_states "4, 3") "    "
                               ((boiling_point units "Kelvin") "3560") "    "
                               ((melting_point units "Kelvin") "1945") "    "
                               (symbol "Ti") "    "
                               ((density units "grams/cubic centimeter")
                                "      4.54    ")
                               "    " (electron_configuration "[Ar] 3d2 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.32")
                               "    " (electronegativity "1.54") "    "
                               ((atomic_radius units "Angstroms") "1.45")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      425.2    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      10.6    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      18.6    ")
                               "    " (ionization_potential "6.82") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.523    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          21.9    ")
                               "  ")
                         "  "
                         (atom "    " (name "Thallium") "    "
                               (atomic_weight "204.383") "    "
                               (atomic_number "81") "    "
                               (oxidation_states "3, 1") "    "
                               ((boiling_point units "Kelvin") "1746") "    "
                               ((melting_point units "Kelvin") "577") "    "
                               (symbol "Tl") "    "
                               ((density units "grams/cubic centimeter")
                                "      11.85    ")
                               "    "
                               (electron_configuration
                                "[Xe] 4f14 5d10 6s2 p1 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.48")
                               "    " (electronegativity "2.04") "    "
                               ((atomic_radius units "Angstroms") "1.71")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      162.09    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      17.2    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      4.27    ")
                               "    " (ionization_potential "6.108") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.129    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          46.1    ")
                               "  ")
                         "  "
                         (atom "    " (name "Thulium") "    "
                               (atomic_weight "168.9342") "    "
                               (atomic_number "69") "    "
                               (oxidation_states "3, 2") "    "
                               ((boiling_point units "Kelvin") "2223") "    "
                               ((melting_point units "Kelvin") "1818") "    "
                               (symbol "Tm") "    "
                               ((density units "grams/cubic centimeter")
                                "      9.32    ")
                               "    "
                               (electron_configuration "[Xe] 4f13 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.56")
                               "    " (electronegativity "1.25") "    "
                               ((atomic_radius units "Angstroms") "1.77")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      191    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      18.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      16.8    ")
                               "    " (ionization_potential "6.184") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.16    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          16.8    ")
                               "  ")
                         "  "
                         (atom "    " (name "Uranium") "    "
                               (atomic_weight "238.029") "    "
                               (atomic_number "92") "    "
                               (oxidation_states "6, 5, 4, 3") "    "
                               ((boiling_point units "Kelvin") "4407") "    "
                               ((melting_point units "Kelvin") "1408") "    "
                               (symbol "U") "    "
                               ((density units "grams/cubic centimeter")
                                "      18.95    ")
                               "    "
                               (electron_configuration "[Rn] 5f3 6d1 7s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.42")
                               "    " (electronegativity "1.38") "    "
                               ((atomic_radius units "Angstroms") "1.38")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      422.58    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      12.5    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      15.48    ")
                               "    " (ionization_potential "6.05") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.12    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          27.6    ")
                               "  ")
                         "  "
                         (atom "    " (name "ununbium") "    "
                               (atomic_weight "277") "    "
                               (atomic_number "112") "    " (symbol "Uub")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d10 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "ununnilium") "    "
                               (atomic_weight "269") "    "
                               (atomic_number "110") "    " (symbol "Uun")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d8 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "unununium") "    "
                               (atomic_weight "272") "    "
                               (atomic_number "111") "    " (symbol "Uuu")
                               "    "
                               (electron_configuration "[Rn] 5f14 6d9 7s2 ")
                               "  ")
                         "  "
                         (atom "    " (name "Vanadium") "    "
                               (atomic_weight "50.9415") "    "
                               (atomic_number "23") "    "
                               (oxidation_states "5, 4, 3, 2") "    "
                               ((boiling_point units "Kelvin") "3650") "    "
                               ((melting_point units "Kelvin") "2163") "    "
                               (symbol "V") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.11    ")
                               "    " (electron_configuration "[Ar] 3d3 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.22")
                               "    " (electronegativity "1.63") "    "
                               ((atomic_radius units "Angstroms") "1.34")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      446.7    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      8.35    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      20.8    ")
                               "    " (ionization_potential "6.74") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.489    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          30.7    ")
                               "  ")
                         "  "
                         (atom "    " (name "Tungsten") "    "
                               (atomic_weight "183.85") "    "
                               (atomic_number "74") "    "
                               (oxidation_states "6, 5, 4, 3, 2") "    "
                               ((boiling_point units "Kelvin") "5825") "    "
                               ((melting_point units "Kelvin") "3695") "    "
                               (symbol "W") "    "
                               ((density units "grams/cubic centimeter")
                                "      19.3    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 5d4 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.3")
                               "    " (electronegativity "2.36") "    "
                               ((atomic_radius units "Angstroms") "1.41")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      422.58    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      9.53    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      35.4    ")
                               "    " (ionization_potential "7.98") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.13    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          174    ")
                               "  ")
                         "  "
                         ((atom state "GAS") "    " (name "Xenon") "    "
                          (atomic_weight "131.29") "    " (atomic_number "54")
                          "    " ((boiling_point units "Kelvin") "165.1")
                          "    " ((melting_point units "Kelvin") "161.39")
                          "    " (symbol "Xe") "    "
                          ((density units "grams/cubic centimeter")
                           "      5.9    ")
                          "    " (electron_configuration "[Kr] 4d10 5s2 p6 ")
                          "    " ((covalent_radius units "Angstroms") "1.31")
                          "    " (electronegativity "2.6") "    "
                          ((atomic_radius units "Angstroms") "1.24") "    "
                          ((heat_of_vaporization units "kilojoules/mole")
                           "      12.64    ")
                          "    "
                          ((atomic_volume units "cubic centimeters/mole")
                           "      42.9    ")
                          "    "
                          ((heat_of_fusion units "kilojoules/mole")
                           "      2.3    ")
                          "    " (ionization_potential "12.13") "    "
                          ((specific_heat_capacity units
                            "Joules/gram/degree Kelvin")
                           "      0.158    ")
                          "    "
                          ((thermal_conductivity units
                            "Watts/meter/degree Kelvin")
                           "          0.00569    ")
                          "  ")
                         "  "
                         (atom "    " (name "Yttrium") "    "
                               (atomic_weight "88.9059") "    "
                               (atomic_number "39") "    "
                               (oxidation_states "3") "    "
                               ((boiling_point units "Kelvin") "3611") "    "
                               ((melting_point units "Kelvin") "1795") "    "
                               (symbol "Y") "    "
                               ((density units "grams/cubic centimeter")
                                "      4.47    ")
                               "    " (electron_configuration "[Kr] 4d1 5s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.62")
                               "    " (electronegativity "1.22") "    "
                               ((atomic_radius units "Angstroms") "1.78")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      363.3    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      19.8    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      17.5    ")
                               "    " (ionization_potential "6.38") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.3    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          17.2    ")
                               "  ")
                         "  "
                         (atom "    " (name "Ytterbium") "    "
                               (atomic_weight "173.04") "    "
                               (atomic_number "70") "    "
                               (oxidation_states "3, 2") "    "
                               ((boiling_point units "Kelvin") "1469")
                               "        "
                               ((melting_point units "Kelvin") "1092") "    "
                               (symbol "Yb") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.97    ")
                               "    "
                               (electron_configuration "[Xe] 4f14 6s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.7")
                               "    " (electronegativity "1.1") "    "
                               ((atomic_radius units "Angstroms") "1.94")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      128    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      24.8    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      7.7    ")
                               "    " (ionization_potential "6.254") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.155    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          34.9    ")
                               "  ")
                         "  "
                         (atom "    " (name "Zinc") "    "
                               (atomic_weight "65.39") "    "
                               (atomic_number "30") "    "
                               (oxidation_states "2") "    "
                               ((boiling_point units "Kelvin") "1180") "    "
                               ((melting_point units "Kelvin") "692.73")
                               "    " (symbol "Zn") "    "
                               ((density units "grams/cubic centimeter")
                                "      7.13    ")
                               "    "
                               (electron_configuration "[Ar] 3d10 4s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.25")
                               "    " (electronegativity "1.65") "    "
                               ((atomic_radius units "Angstroms") "1.38")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      115.3    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      9.2    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      7.38    ")
                               "    " (ionization_potential "9.394") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.388    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          116    ")
                               "  ")
                         "  "
                         (atom "    " (name "Zirconium") "    "
                               (atomic_weight "91.224") "    "
                               (atomic_number "40") "    "
                               (oxidation_states "4") "    "
                               ((boiling_point units "Kelvin") "4682") "    "
                               ((melting_point units "Kelvin") "2128") "    "
                               (symbol "Zr") "    "
                               ((density units "grams/cubic centimeter")
                                "      6.51    ")
                               "    " (electron_configuration "[Kr] 4d2 5s2 ")
                               "    "
                               ((covalent_radius units "Angstroms") "1.45")
                               "    " (electronegativity "1.33") "    "
                               ((atomic_radius units "Angstroms") "1.6")
                               "    "
                               ((heat_of_vaporization units "kilojoules/mole")
                                "      590.5    ")
                               "    "
                               ((atomic_volume units "cubic centimeters/mole")
                                "      14.1    ")
                               "    "
                               ((heat_of_fusion units "kilojoules/mole")
                                "      21    ")
                               "    " (ionization_potential "6.84") "    "
                               ((specific_heat_capacity units
                                 "Joules/gram/degree Kelvin")
                                "      0.278    ")
                               "    "
                               ((thermal_conductivity units
                                 "Watts/meter/degree Kelvin")
                                "          22.7    ")
                               "  ")
                         "  ")))