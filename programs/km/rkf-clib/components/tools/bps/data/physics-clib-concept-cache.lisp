;;;
;;; $Id: physics-clib-concept-cache.lisp,v 1.1 2008/10/21 20:59:52 jchaw Exp $
;;;

(unless (find-package :km)   (make-package :km))
(unless (find-package :aura) (make-package :aura))
(in-package :km)
(setq *using-km-package* t)
(defparameter *physics-clib-persistent-hash* 
'((|Duration-Value| (|_Duration-Value37745|
                                                          (((|_Duration-Value37745|
                                                             |instance-of|
                                                             |Duration-Value|))
                                                           T
                                                           NIL)))
(|Forget| (|_Forget25016|
           (((|_Move25025| |instance-of| |Move|)
             (|_Time-Interval25024| |instance-of| |Time-Interval|)
             (|_Information25023| |instance-of| |Information|)
             (|_Information25023| |instance-of| |Spatial-Entity|)
             (|_Forget25016| |instance-of| |Forget|)
             (|_Tangible-Entity25021| |instance-of| |Tangible-Entity|)
             (|_Forget25016| |actions| |_Forget25016|)
             (|_Forget25016| |preparatory-event| |_Move25025|)
             (|_Forget25016| |primitive-actions| |_Forget25016|)
             (|_Forget25016| |time-during| |_Time-Interval25024|)
             (|_Forget25016| |object| |_Information25023|)
             (|_Forget25016| |agent| |_Tangible-Entity25021|))
            T NIL)))
(|Property| (|_Property37953|
             (((|_Property37953| |instance-of| |Property|)) T NIL)))
(|Close| (|_Close23983|
          (((|_Portal24024| |instance-of| |Portal|)
            (|_Time-Interval24028| |instance-of| |Time-Interval|)
            (|_Spatial-Entity24023| |instance-of| |Spatial-Entity|)
            (|_Move24021| |instance-of| |Move|)
            (|_Close24018| |instance-of| |Close|)
            (|_Time-Interval24017| |instance-of| |Time-Interval|)
            (|_Be-Open24016| |instance-of| |Be-Open|)
            (|_Portal23992| |instance-of| |Portal|)
            (|_Portal-Covering24000| |instance-of| |Portal-Covering|)
            (|_Time-Interval24006| |instance-of| |Time-Interval|)
            (|_Spatial-Entity24002| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity23999| |instance-of| |Tangible-Entity|)
            (|_Move23998| |instance-of| |Move|)
            (|_Open23995| |instance-of| |Open|)
            (|_Time-Interval23994| |instance-of| |Time-Interval|)
            (|_Spatial-Entity23991| |instance-of| |Spatial-Entity|)
            (|_Be-Closed23993| |instance-of| |Be-Closed|)
            (|_Close23983| |instance-of| |Close|)
            (|_Tangible-Entity23989| |instance-of| |Tangible-Entity|)
            (|_Be-Closed23993| |object| |_Spatial-Entity23991|)
            (|_Spatial-Entity24023| |plays| |_Portal24024|)
            (|_Be-Open24016| |actions| |_Be-Open24016|)
            (|_Be-Open24016| |primitive-actions| |_Be-Open24016|)
            (|_Be-Open24016| |time-during| |_Time-Interval24028|)
            (|_Be-Open24016| |object| |_Spatial-Entity24023|)
            (|_Open23995| |actions| |_Open23995|)
            (|_Open23995| |preparatory-event| |_Move24021|)
            (|_Open23995| |preparatory-event| |_Close24018|)
            (|_Open23995| |primitive-actions| |_Open23995|)
            (|_Open23995| |time-during| |_Time-Interval24017|)
            (|_Open23995| |object| |_Spatial-Entity23991|)
            (|_Open23995| |defeats| |_Be-Closed23993|)
            (|_Open23995| |resulting-state| |_Be-Open24016|)
            (|_Open23995| |instrument| |_Tangible-Entity23989|)
            (|_Spatial-Entity23991| |plays| |_Portal23992|)
            (|_Tangible-Entity23999| |plays| |_Portal-Covering24000|)
            (|_Be-Closed23993| |actions| |_Be-Closed23993|)
            (|_Be-Closed23993| |primitive-actions| |_Be-Closed23993|)
            (|_Be-Closed23993| |time-during| |_Time-Interval24006|)
            (|_Be-Closed23993| |object| |_Spatial-Entity24002|)
            (|_Be-Closed23993| |instrument| |_Tangible-Entity23999|)
            (|_Close23983| |actions| |_Close23983|)
            (|_Close23983| |preparatory-event| |_Move23998|)
            (|_Close23983| |preparatory-event| |_Open23995|)
            (|_Close23983| |primitive-actions| |_Close23983|)
            (|_Close23983| |time-during| |_Time-Interval23994|)
            (|_Close23983| |object| |_Spatial-Entity23991|)
            (|_Close23983| |resulting-state| |_Be-Closed23993|)
            (|_Close23983| |instrument| |_Tangible-Entity23989|))
           T NIL)))
(|Light| (|_Light34655|
          (((|_Light34655| |instance-of| |Light|)
            (|_Move34656| |instance-of| |Move|)
            (|_Light34655| |object-of| |_Move34656|))
           T NIL)))
(|Assurance| (|_Assurance34789|
              (((|_Language34792| |instance-of| |Language|)
                (|_Assurance34789| |instance-of| |Assurance|)
                (|_Thing34791| |instance-of| |Thing|)
                (|_Assurance34789| |information-language|
                 |_Language34792|)
                (|_Assurance34789| |information-content|
                 |_Thing34791|))
               T NIL)))
(|Organizing| (|_Organizing758|
               (((|_Organizing758| |instance-of| |Organizing|)
                 (|_Time-Interval764| |instance-of| |Time-Interval|)
                 (|_Organizing758| |actions| |_Organizing758|)
                 (|_Organizing758| |primitive-actions|
                  |_Organizing758|)
                 (|_Organizing758| |time-during| |_Time-Interval764|))
                T NIL)))
(|Sell| (|_Sell1584|
         (((|_Move1710| |instance-of| |Move|)
           (|_Obtain1709| |instance-of| |Obtain|)
           (|_Time-Interval1708| |instance-of| |Time-Interval|)
           (|_Entity1705| |instance-of| |Tangible-Entity|)
           (|_Move1702| |instance-of| |Move|)
           (|_Obtain1701| |instance-of| |Obtain|)
           (|_Time-Interval1700| |instance-of| |Time-Interval|)
           (|_Move1672| |instance-of| |Move|)
           (|_Obtain1671| |instance-of| |Obtain|)
           (|_Time-Interval1670| |instance-of| |Time-Interval|)
           (|_Entity1612| |instance-of| |Tangible-Entity|)
           (|_Move1647| |instance-of| |Move|)
           (|_Obtain1646| |instance-of| |Obtain|)
           (|_Time-Interval1645| |instance-of| |Time-Interval|)
           (|_Move1609| |instance-of| |Move|)
           (|_Obtain1608| |instance-of| |Obtain|)
           (|_Time-Interval1607| |instance-of| |Time-Interval|)
           (|_Resource1602| |instance-of| |Resource|)
           (|_Resource1601| |instance-of| |Resource|)
           (|_Worth-Value1600| |instance-of| |Worth-Value|)
           (|_Move1599| |instance-of| |Move|)
           (|_Obtain1598| |instance-of| |Obtain|)
           (|_Time-Interval1597| |instance-of| |Time-Interval|)
           (|_Entity1595| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity1593| |instance-of| |Tangible-Entity|)
           (|_Transfer1596| |instance-of| |Transfer|)
           (|_Tangible-Entity1591| |instance-of| |Tangible-Entity|)
           (|_Sell1584| |instance-of| |Sell|)
           (|_Money1592| |instance-of| |Money|)
           (|_Money1592| |instance-of| |Spatial-Entity|)
           (|_Obtain1701| |actions| |_Obtain1701|)
           (|_Obtain1701| |preparatory-event| |_Move1710|)
           (|_Obtain1701| |preparatory-event| |_Obtain1709|)
           (|_Obtain1701| |primitive-actions| |_Obtain1701|)
           (|_Obtain1701| |time-during| |_Time-Interval1708|)
           (|_Obtain1701| |object| |_Money1592|)
           (|_Obtain1701| |recipient| |_Entity1705|)
           (|_Obtain1701| |agent| |_Entity1705|)
           (|_Obtain1646| |actions| |_Obtain1646|)
           (|_Obtain1646| |preparatory-event| |_Move1702|)
           (|_Obtain1646| |preparatory-event| |_Obtain1701|)
           (|_Obtain1646| |primitive-actions| |_Obtain1646|)
           (|_Obtain1646| |time-during| |_Time-Interval1700|)
           (|_Obtain1646| |object| |_Money1592|)
           (|_Obtain1646| |recipient| |_Tangible-Entity1593|)
           (|_Obtain1646| |agent| |_Tangible-Entity1593|)
           (|_Obtain1608| |actions| |_Obtain1608|)
           (|_Obtain1608| |preparatory-event| |_Move1672|)
           (|_Obtain1608| |preparatory-event| |_Obtain1671|)
           (|_Obtain1608| |primitive-actions| |_Obtain1608|)
           (|_Obtain1608| |time-during| |_Time-Interval1670|)
           (|_Obtain1608| |object| |_Entity1595|)
           (|_Obtain1608| |recipient| |_Entity1612|)
           (|_Obtain1608| |agent| |_Entity1612|)
           (|_Tangible-Entity1591| |object-of| |_Move1599|)
           (|_Tangible-Entity1591| |object-of| |_Move1609|)
           (|_Transfer1596| |actions-of| |_Transfer1596|)
           (|_Transfer1596| |preparatory-event| |_Move1647|)
           (|_Transfer1596| |preparatory-event| |_Obtain1646|)
           (|_Transfer1596| |primitive-actions-of| |_Transfer1596|)
           (|_Transfer1596| |time-during| |_Time-Interval1645|)
           (|_Transfer1596| |object| |_Money1592|)
           (|_Transfer1596| |recipient| |_Tangible-Entity1591|)
           (|_Transfer1596| |agent| |_Tangible-Entity1593|)
           (|_Transfer1596| |donor| |_Tangible-Entity1593|)
           (|_Obtain1598| |actions| |_Obtain1598|)
           (|_Obtain1598| |preparatory-event| |_Move1609|)
           (|_Obtain1598| |preparatory-event| |_Obtain1608|)
           (|_Obtain1598| |primitive-actions| |_Obtain1598|)
           (|_Obtain1598| |time-during| |_Time-Interval1607|)
           (|_Obtain1598| |object| |_Entity1595|)
           (|_Obtain1598| |recipient| |_Tangible-Entity1591|)
           (|_Obtain1598| |agent| |_Tangible-Entity1591|)
           (|_Tangible-Entity1591| |abuts| |_Money1592|)
           (|_Money1592| |purpose| |_Resource1602|)
           (|_Money1592| |purpose| |_Resource1601|)
           (|_Money1592| |worth| |_Worth-Value1600|)
           (|_Sell1584| |actions| |_Sell1584|)
           (|_Sell1584| |actions| |_Transfer1596|)
           (|_Sell1584| |all-subevents| |_Transfer1596|)
           (|_Sell1584| |preparatory-event| |_Move1599|)
           (|_Sell1584| |preparatory-event| |_Obtain1598|)
           (|_Sell1584| |primitive-actions| |_Transfer1596|)
           (|_Sell1584| |time-during| |_Time-Interval1597|)
           (|_Sell1584| |object| |_Entity1595|)
           (|_Sell1584| |recipient| |_Tangible-Entity1593|)
           (|_Sell1584| |first-subevent| |_Transfer1596|)
           (|_Sell1584| |subevent| |_Transfer1596|)
           (|_Sell1584| |agent| |_Tangible-Entity1591|)
           (|_Sell1584| |donor| |_Tangible-Entity1591|)
           (|_Sell1584| |instrument| |_Money1592|))
          T NIL)))
(|Rocket| (|_Rocket34627|
           (((|_Rocket34627| |instance-of| |Rocket|)) T NIL)))
(|Be-Stored| (|_Be-Stored50|
              (((|_Time-Interval140| |instance-of| |Time-Interval|)
                (|_Spatial-Entity136| |instance-of| |Tangible-Entity|)
                (|_Move134| |instance-of| |Move|)
                (|_Unblock132| |instance-of| |Unblock|)
                (|_Time-Interval131| |instance-of| |Time-Interval|)
                (|_Be-Blocked130| |instance-of| |Be-Blocked|)
                (|_Move102| |instance-of| |Move|)
                (|_Block100| |instance-of| |Block|)
                (|_Time-Interval99| |instance-of| |Time-Interval|)
                (|_Time-Interval88| |instance-of| |Time-Interval|)
                (|_Spatial-Entity81| |instance-of| |Tangible-Entity|)
                (|_Move76| |instance-of| |Move|)
                (|_Unblock74| |instance-of| |Unblock|)
                (|_Time-Interval73| |instance-of| |Time-Interval|)
                (|_Spatial-Entity66| |instance-of| |Spatial-Entity|)
                (|_Spatial-Entity65| |instance-of| |Spatial-Entity|)
                (|_Tangible-Entity72| |instance-of| |Tangible-Entity|)
                (|_Be-Blocked71| |instance-of| |Be-Blocked|)
                (|_Be-Contained60| |instance-of| |Be-Contained|)
                (|_Be-Contained60| |instance-of| |Block|)
                (|_Container58| |instance-of| |Container|)
                (|_Time-Interval55| |instance-of| |Time-Interval|)
                (|_Tangible-Entity54| |instance-of| |Tangible-Entity|)
                (|_Be-Stored50| |instance-of| |Be-Stored|)
                (|_Entity53| |instance-of| |Tangible-Entity|)
                (|_Be-Blocked130| |actions| |_Be-Blocked130|)
                (|_Be-Blocked130| |primitive-actions| |_Be-Blocked130|)
                (|_Be-Blocked130| |time-during| |_Time-Interval140|)
                (|_Be-Blocked130| |object| |_Spatial-Entity136|)
                (|_Block100| |actions| |_Block100|)
                (|_Block100| |preparatory-event| |_Move134|)
                (|_Block100| |preparatory-event| |_Unblock132|)
                (|_Block100| |primitive-actions| |_Block100|)
                (|_Block100| |time-during| |_Time-Interval131|)
                (|_Block100| |base| |_Tangible-Entity72|)
                (|_Block100| |object| |_Entity53|)
                (|_Block100| |resulting-state| |_Be-Blocked130|)
                (|_Be-Blocked71| |object| |_Entity53|)
                (|_Unblock74| |actions| |_Unblock74|)
                (|_Unblock74| |preparatory-event| |_Move102|)
                (|_Unblock74| |preparatory-event| |_Block100|)
                (|_Unblock74| |primitive-actions| |_Unblock74|)
                (|_Unblock74| |time-during| |_Time-Interval99|)
                (|_Unblock74| |base| |_Tangible-Entity72|)
                (|_Unblock74| |object| |_Entity53|)
                (|_Unblock74| |defeats| |_Be-Blocked71|)
                (|_Be-Blocked71| |actions| |_Be-Blocked71|)
                (|_Be-Blocked71| |primitive-actions| |_Be-Blocked71|)
                (|_Be-Blocked71| |time-during| |_Time-Interval88|)
                (|_Be-Blocked71| |object| |_Spatial-Entity81|)
                (|_Be-Contained60| |actions| |_Be-Contained60|)
                (|_Be-Contained60| |preparatory-event| |_Move76|)
                (|_Be-Contained60| |preparatory-event| |_Unblock74|)
                (|_Be-Contained60| |primitive-actions|
                 |_Be-Contained60|)
                (|_Be-Contained60| |time-during| |_Time-Interval73|)
                (|_Be-Contained60| |destination| |_Spatial-Entity66|)
                (|_Be-Contained60| |origin| |_Spatial-Entity65|)
                (|_Be-Contained60| |base| |_Tangible-Entity72|)
                (|_Be-Contained60| |object| |_Entity53|)
                (|_Be-Contained60| |resulting-state| |_Be-Blocked71|)
                (|_Be-Contained60| |instrument| |_Tangible-Entity54|)
                (|_Entity53| |is-inside| |_Tangible-Entity54|)
                (|_Container58| |in-event| |_Be-Contained60|)
                (|_Container58| |content| |_Entity53|)
                (|_Tangible-Entity54| |plays| |_Container58|)
                (|_Be-Stored50| |actions| |_Be-Stored50|)
                (|_Be-Stored50| |primitive-actions| |_Be-Stored50|)
                (|_Be-Stored50| |time-during| |_Time-Interval55|)
                (|_Be-Stored50| |base| |_Tangible-Entity54|)
                (|_Be-Stored50| |object| |_Entity53|))
               T NIL)))
(|Conceptual-Entity| (|_Conceptual-Entity34738|
                      (((|_Conceptual-Entity34738| |instance-of|
                         |Conceptual-Entity|))
                       T NIL)))
(|Bowling-Ball| (|_Bowling-Ball34605|
                 (((|_Bowling-Ball34605| |instance-of| |Bowling-Ball|))
                  T NIL)))
(|Point| (|_Point34706|
          (((|_Point34706| |instance-of| |Point|)) T NIL)))
(|Truck| (|_Truck34635|
          (((|_Truck34635| |instance-of| |Truck|)) T NIL)))
(|Committee| (|_Committee34867|
              (((|_Committee34867| |instance-of| |Committee|)
                (|_Number34869| |instance-of| |Number|)
                (|_Committee34867| |number-of-elements|
                 |_Number34869|))
               T NIL)))
(|Power-Constant| (|_Power-Constant37853|
                   (((|_Power-Constant37853| |instance-of|
                      |Power-Constant|))
                    T NIL)))
(|UoM-Rate| (|_UoM-Rate37440|
             (((|_UoM-Rate37440| |instance-of| |UoM-Rate|)) T NIL)))
(|Worth-Scale| (|_Worth-Scale37492|
                (((|_Worth-Scale37492| |instance-of| |Worth-Scale|)
                  (|_Number37494| |instance-of| |Number|)
                  (|_Worth-Scale37492| |number-of-elements|
                   |_Number37494|))
                 T NIL)))
(|Truth-Constant| (|_Truth-Constant37811|
                   (((|_Truth-Constant37811| |instance-of|
                      |Truth-Constant|))
                    T NIL)))
(|Direction-Constant| (|_Direction-Constant37893|
                       (((|_Direction-Constant37893| |instance-of|
                          |Direction-Constant|))
                        T NIL)))
(|Cell| (|_Cell34031| (((|_Cell34031| |instance-of| |Cell|)) T NIL)))
(|Feed| (|_Feed1420|
         (((|_Move1519| |instance-of| |Move|)
           (|_Obtain1518| |instance-of| |Obtain|)
           (|_Time-Interval1517| |instance-of| |Time-Interval|)
           (|_Entity1514| |instance-of| |Tangible-Entity|)
           (|_Move1488| |instance-of| |Move|)
           (|_Obtain1487| |instance-of| |Obtain|)
           (|_Time-Interval1486| |instance-of| |Time-Interval|)
           (|_Food1433| |instance-of| |Food|)
           (|_Move1442| |instance-of| |Move|)
           (|_Time-Interval1441| |instance-of| |Time-Interval|)
           (|_Resource1439| |instance-of| |Resource|)
           (|_Move1437| |instance-of| |Move|)
           (|_Obtain1436| |instance-of| |Obtain|)
           (|_Time-Interval1435| |instance-of| |Time-Interval|)
           (|_Tangible-Entity1432| |instance-of| |Tangible-Entity|)
           (|_Living-Entity1430| |instance-of| |Living-Entity|)
           (|_Replenish1434| |instance-of| |Replenish|)
           (|_Feed1420| |instance-of| |Feed|)
           (|_Living-Entity1429| |instance-of| |Living-Entity|)
           (|_Obtain1487| |actions| |_Obtain1487|)
           (|_Obtain1487| |preparatory-event| |_Move1519|)
           (|_Obtain1487| |preparatory-event| |_Obtain1518|)
           (|_Obtain1487| |primitive-actions| |_Obtain1487|)
           (|_Obtain1487| |time-during| |_Time-Interval1517|)
           (|_Obtain1487| |object| |_Tangible-Entity1432|)
           (|_Obtain1487| |recipient| |_Entity1514|)
           (|_Obtain1487| |agent| |_Entity1514|)
           (|_Obtain1436| |actions| |_Obtain1436|)
           (|_Obtain1436| |preparatory-event| |_Move1488|)
           (|_Obtain1436| |preparatory-event| |_Obtain1487|)
           (|_Obtain1436| |primitive-actions| |_Obtain1436|)
           (|_Obtain1436| |time-during| |_Time-Interval1486|)
           (|_Obtain1436| |object| |_Tangible-Entity1432|)
           (|_Obtain1436| |recipient| |_Living-Entity1429|)
           (|_Obtain1436| |agent| |_Living-Entity1429|)
           (|_Tangible-Entity1432| |destination-of| |_Move1442|)
           (|_Tangible-Entity1432| |plays| |_Resource1439|)
           (|_Tangible-Entity1432| |plays| |_Food1433|)
           (|_Replenish1434| |actions| |_Replenish1434|)
           (|_Replenish1434| |preparatory-event| |_Move1442|)
           (|_Replenish1434| |primitive-actions| |_Replenish1434|)
           (|_Replenish1434| |time-during| |_Time-Interval1441|)
           (|_Replenish1434| |base| |_Resource1439|)
           (|_Replenish1434| |object| |_Tangible-Entity1432|)
           (|_Feed1420| |actions| |_Feed1420|)
           (|_Feed1420| |preparatory-event| |_Move1437|)
           (|_Feed1420| |preparatory-event| |_Obtain1436|)
           (|_Feed1420| |primitive-actions| |_Feed1420|)
           (|_Feed1420| |time-during| |_Time-Interval1435|)
           (|_Feed1420| |object| |_Tangible-Entity1432|)
           (|_Feed1420| |recipient| |_Living-Entity1430|)
           (|_Feed1420| |enables| |_Replenish1434|)
           (|_Feed1420| |agent| |_Living-Entity1429|)
           (|_Feed1420| |donor| |_Living-Entity1429|))
          T NIL)))
(|Force-Constant| (|_Force-Constant37927|
                   (((|_Force-Constant37927| |instance-of|
                      |Force-Constant|))
                    T NIL)))
(|Orientation-Constant| (|_Orientation-Constant37859|
                         (((|_Orientation-Constant37859| |instance-of|
                            |Orientation-Constant|))
                          T NIL)))
(|Load| (|_Load22997|
         (((|_Move23101| |instance-of| |Move|)
           (|_Time-Interval23100| |instance-of| |Time-Interval|)
           (|_Tangible-Entity23099| |instance-of| |Tangible-Entity|)
           (|_Acceleration-Magnitude-Value23098| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value23082| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value23097| |instance-of| |Length-Value|)
           (|_Duration-Value23096| |instance-of| |Duration-Value|)
           (|_Speed-Value23095| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value23080| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value23094| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23091| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23093| |instance-of| |Speed-Value|)
           (|_Speed-Value23092| |instance-of| |Speed-Value|)
           (|_Speed-Value23090| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23087| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23089| |instance-of| |Speed-Value|)
           (|_Speed-Value23088| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23078| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value23086| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23085| |instance-of| |Length-Value|)
           (|_Speed-Value23084| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value23083| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23081| |instance-of| |Length-Value|)
           (|_Speed-Value23079| |instance-of| |Speed-Value|)
           (|_Move23077| |instance-of| |Move|)
           (|_Time-Interval23076| |instance-of| |Time-Interval|)
           (|_Acceleration-Magnitude-Value23075| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value23059| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value23074| |instance-of| |Length-Value|)
           (|_Duration-Value23073| |instance-of| |Duration-Value|)
           (|_Speed-Value23072| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value23057| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value23071| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23068| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23070| |instance-of| |Speed-Value|)
           (|_Speed-Value23069| |instance-of| |Speed-Value|)
           (|_Speed-Value23067| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23064| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23066| |instance-of| |Speed-Value|)
           (|_Speed-Value23065| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23055| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value23063| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23062| |instance-of| |Length-Value|)
           (|_Speed-Value23061| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value23060| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23058| |instance-of| |Length-Value|)
           (|_Speed-Value23056| |instance-of| |Speed-Value|)
           (|_Move23054| |instance-of| |Move|)
           (|_Shut-Out23052| |instance-of| |Shut-Out|)
           (|_Time-Interval23051| |instance-of| |Time-Interval|)
           (|_Portal23050| |instance-of| |Portal|)
           (|_Container23049| |instance-of| |Container|)
           (|_Move23048| |instance-of| |Move|)
           (|_Deactivate23046| |instance-of| |Deactivate|)
           (|_Time-Interval23045| |instance-of| |Time-Interval|)
           (|_Be-Activated23044| |instance-of| |Be-Activated|)
           (|_Time-Interval23043| |instance-of| |Time-Interval|)
           (|_Spatial-Entity23042| |instance-of| |Spatial-Entity|)
           (|_Spatial-Entity23041| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity23040| |instance-of| |Tangible-Entity|)
           (|_Tangible-Entity23039| |instance-of| |Tangible-Entity|)
           (|_Angle-Value23038| |instance-of| |Angle-Value|)
           (|_Angle-Value23037| |instance-of| |Angle-Value|)
           (|_Angle-Value23036| |instance-of| |Angle-Value|)
           (|_Angle-Value23035| |instance-of| |Angle-Value|)
           (|_Angle-Value23034| |instance-of| |Angle-Value|)
           (|_Move23030| |instance-of| |Move|)
           (|_Move23029| |instance-of| |Move|)
           (|_Admit23028| |instance-of| |Admit|)
           (|_Time-Interval23027| |instance-of| |Time-Interval|)
           (|_Spatial-Entity23023| |instance-of| |Spatial-Entity|)
           (|_Spatial-Entity23022| |instance-of| |Spatial-Entity|)
           (|_Spatial-Entity23024| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity23025| |instance-of| |Tangible-Entity|)
           (|_Tangible-Entity23021| |instance-of| |Tangible-Entity|)
           (|_Activate23020| |instance-of| |Activate|)
           (|_Be-Contained23019| |instance-of| |Be-Contained|)
           (|_Acceleration-Magnitude-Value23018| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value23002| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value23017| |instance-of| |Length-Value|)
           (|_Duration-Value23016| |instance-of| |Duration-Value|)
           (|_Speed-Value23015| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value23000| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value23014| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23011| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23013| |instance-of| |Speed-Value|)
           (|_Speed-Value23012| |instance-of| |Speed-Value|)
           (|_Speed-Value23010| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23007| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23009| |instance-of| |Speed-Value|)
           (|_Speed-Value23008| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value22998| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value23006| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23005| |instance-of| |Length-Value|)
           (|_Speed-Value23004| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value23003| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23001| |instance-of| |Length-Value|)
           (|_Load22997| |instance-of| |Load|)
           (|_Speed-Value22999| |instance-of| |Speed-Value|)
           (|_Move23030| |actions| |_Move23030|)
           (|_Move23030| |preparatory-event| |_Move23101|)
           (|_Move23030| |primitive-actions| |_Move23030|)
           (|_Move23030| |time-during| |_Time-Interval23100|)
           (|_Move23030| |object| |_Tangible-Entity23099|)
           (|_Move23030| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value23098|)
           (|_Move23030| |acceleration|
            |_Acceleration-Vector-Value23082|)
           (|_Move23030| |distance| |_Length-Value23097|)
           (|_Move23030| |duration| |_Duration-Value23096|)
           (|_Move23030| |speed| |_Speed-Value23095|)
           (|_Move23030| |displacement|
            |_Displacement-Vector-Value23080|)
           (|_Move23030| |final-speed| |_Speed-Value23094|)
           (|_Move23030| |final-velocity|
            |_Velocity-Vector-Value23091|)
           (|_Move23030| |final-x-speed| |_Speed-Value23093|)
           (|_Move23030| |final-y-speed| |_Speed-Value23092|)
           (|_Move23030| |initial-speed| |_Speed-Value23090|)
           (|_Move23030| |initial-velocity|
            |_Velocity-Vector-Value23087|)
           (|_Move23030| |initial-x-speed| |_Speed-Value23089|)
           (|_Move23030| |initial-y-speed| |_Speed-Value23088|)
           (|_Move23030| |velocity| |_Velocity-Vector-Value23078|)
           (|_Move23030| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23086|)
           (|_Move23030| |x-distance| |_Length-Value23085|)
           (|_Move23030| |x-speed| |_Speed-Value23084|)
           (|_Move23030| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23083|)
           (|_Move23030| |y-distance| |_Length-Value23081|)
           (|_Move23030| |y-speed| |_Speed-Value23079|)
           (|_Move23029| |actions| |_Move23029|)
           (|_Move23029| |preparatory-event| |_Move23077|)
           (|_Move23029| |primitive-actions| |_Move23029|)
           (|_Move23029| |time-during| |_Time-Interval23076|)
           (|_Move23029| |destination| |_Spatial-Entity23022|)
           (|_Move23029| |object| |_Tangible-Entity23021|)
           (|_Move23029| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value23075|)
           (|_Move23029| |acceleration|
            |_Acceleration-Vector-Value23059|)
           (|_Move23029| |distance| |_Length-Value23074|)
           (|_Move23029| |duration| |_Duration-Value23073|)
           (|_Move23029| |speed| |_Speed-Value23072|)
           (|_Move23029| |displacement|
            |_Displacement-Vector-Value23057|)
           (|_Move23029| |final-speed| |_Speed-Value23071|)
           (|_Move23029| |final-velocity|
            |_Velocity-Vector-Value23068|)
           (|_Move23029| |final-x-speed| |_Speed-Value23070|)
           (|_Move23029| |final-y-speed| |_Speed-Value23069|)
           (|_Move23029| |initial-speed| |_Speed-Value23067|)
           (|_Move23029| |initial-velocity|
            |_Velocity-Vector-Value23064|)
           (|_Move23029| |initial-x-speed| |_Speed-Value23066|)
           (|_Move23029| |initial-y-speed| |_Speed-Value23065|)
           (|_Move23029| |velocity| |_Velocity-Vector-Value23055|)
           (|_Move23029| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23063|)
           (|_Move23029| |x-distance| |_Length-Value23062|)
           (|_Move23029| |x-speed| |_Speed-Value23061|)
           (|_Move23029| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23060|)
           (|_Move23029| |y-distance| |_Length-Value23058|)
           (|_Move23029| |y-speed| |_Speed-Value23056|)
           (|_Admit23028| |actions| |_Admit23028|)
           (|_Admit23028| |preparatory-event| |_Move23054|)
           (|_Admit23028| |preparatory-event| |_Shut-Out23052|)
           (|_Admit23028| |primitive-actions| |_Admit23028|)
           (|_Admit23028| |time-during| |_Time-Interval23051|)
           (|_Admit23028| |base| |_Tangible-Entity23025|)
           (|_Admit23028| |object| |_Tangible-Entity23021|)
           (|_Spatial-Entity23022| |is-outside|
            |_Tangible-Entity23025|)
           (|_Spatial-Entity23024| |plays| |_Portal23050|)
           (|_Tangible-Entity23025| |encloses| |_Spatial-Entity23023|)
           (|_Tangible-Entity23025| |base-of| |_Be-Contained23019|)
           (|_Tangible-Entity23025| |plays| |_Container23049|)
           (|_Tangible-Entity23025| |has-region|
            |_Spatial-Entity23024|)
           (|_Tangible-Entity23021| |destination-of| |_Move23030|)
           (|_Activate23020| |actions| |_Activate23020|)
           (|_Activate23020| |preparatory-event| |_Move23048|)
           (|_Activate23020| |preparatory-event| |_Deactivate23046|)
           (|_Activate23020| |primitive-actions| |_Activate23020|)
           (|_Activate23020| |time-during| |_Time-Interval23045|)
           (|_Activate23020| |object| |_Tangible-Entity23025|)
           (|_Activate23020| |resulting-state| |_Be-Activated23044|)
           (|_Be-Contained23019| |actions| |_Be-Contained23019|)
           (|_Be-Contained23019| |primitive-actions|
            |_Be-Contained23019|)
           (|_Be-Contained23019| |time-during| |_Time-Interval23043|)
           (|_Be-Contained23019| |destination| |_Spatial-Entity23042|)
           (|_Be-Contained23019| |origin| |_Spatial-Entity23041|)
           (|_Be-Contained23019| |base| |_Tangible-Entity23040|)
           (|_Be-Contained23019| |object| |_Tangible-Entity23039|)
           (|_Acceleration-Vector-Value23002| |x-component-slot|
            |x-acceleration-magnitude|)
           (|_Acceleration-Vector-Value23002| |y-component-slot|
            |y-acceleration-magnitude|)
           (|_Acceleration-Vector-Value23002| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value23018|)
           (|_Acceleration-Vector-Value23002| |direction|
            |_Angle-Value23038|)
           (|_Displacement-Vector-Value23000| |x-component-slot|
            |x-distance|)
           (|_Displacement-Vector-Value23000| |y-component-slot|
            |y-distance|)
           (|_Displacement-Vector-Value23000| |direction|
            |_Angle-Value23037|)
           (|_Displacement-Vector-Value23000| |distance|
            |_Length-Value23017|)
           (|_Velocity-Vector-Value23011| |x-component-slot| |x-speed|)
           (|_Velocity-Vector-Value23011| |y-component-slot| |y-speed|)
           (|_Velocity-Vector-Value23011| |direction|
            |_Angle-Value23036|)
           (|_Velocity-Vector-Value23011| |speed| |_Speed-Value23014|)
           (|_Speed-Value23013| |x-speed-of|
            |_Velocity-Vector-Value23011|)
           (|_Speed-Value23012| |y-speed-of|
            |_Velocity-Vector-Value23011|)
           (|_Velocity-Vector-Value23007| |x-component-slot| |x-speed|)
           (|_Velocity-Vector-Value23007| |y-component-slot| |y-speed|)
           (|_Velocity-Vector-Value23007| |direction|
            |_Angle-Value23035|)
           (|_Velocity-Vector-Value23007| |speed| |_Speed-Value23010|)
           (|_Speed-Value23009| |x-speed-of|
            |_Velocity-Vector-Value23007|)
           (|_Speed-Value23008| |y-speed-of|
            |_Velocity-Vector-Value23007|)
           (|_Velocity-Vector-Value22998| |x-component-slot| |x-speed|)
           (|_Velocity-Vector-Value22998| |y-component-slot| |y-speed|)
           (|_Velocity-Vector-Value22998| |direction|
            |_Angle-Value23034|)
           (|_Velocity-Vector-Value22998| |speed| |_Speed-Value23015|)
           (|_Acceleration-Magnitude-Value23006|
            |x-acceleration-magnitude-of|
            |_Acceleration-Vector-Value23002|)
           (|_Length-Value23005| |x-distance-of|
            |_Displacement-Vector-Value23000|)
           (|_Speed-Value23004| |x-speed-of|
            |_Velocity-Vector-Value22998|)
           (|_Acceleration-Magnitude-Value23003|
            |y-acceleration-magnitude-of|
            |_Acceleration-Vector-Value23002|)
           (|_Length-Value23001| |y-distance-of|
            |_Displacement-Vector-Value23000|)
           (|_Speed-Value22999| |y-speed-of|
            |_Velocity-Vector-Value22998|)
           (|_Load22997| |actions| |_Load22997|)
           (|_Load22997| |preparatory-event| |_Move23030|)
           (|_Load22997| |preparatory-event| |_Move23029|)
           (|_Load22997| |preparatory-event| |_Admit23028|)
           (|_Load22997| |primitive-actions| |_Load22997|)
           (|_Load22997| |time-during| |_Time-Interval23027|)
           (|_Load22997| |destination| |_Spatial-Entity23023|)
           (|_Load22997| |origin| |_Spatial-Entity23022|)
           (|_Load22997| |path| |_Spatial-Entity23024|)
           (|_Load22997| |base| |_Tangible-Entity23025|)
           (|_Load22997| |object| |_Tangible-Entity23021|)
           (|_Load22997| |causes| |_Activate23020|)
           (|_Load22997| |resulting-state| |_Be-Contained23019|)
           (|_Load22997| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value23018|)
           (|_Load22997| |acceleration|
            |_Acceleration-Vector-Value23002|)
           (|_Load22997| |distance| |_Length-Value23017|)
           (|_Load22997| |duration| |_Duration-Value23016|)
           (|_Load22997| |speed| |_Speed-Value23015|)
           (|_Load22997| |displacement|
            |_Displacement-Vector-Value23000|)
           (|_Load22997| |final-speed| |_Speed-Value23014|)
           (|_Load22997| |final-velocity|
            |_Velocity-Vector-Value23011|)
           (|_Load22997| |final-x-speed| |_Speed-Value23013|)
           (|_Load22997| |final-y-speed| |_Speed-Value23012|)
           (|_Load22997| |initial-speed| |_Speed-Value23010|)
           (|_Load22997| |initial-velocity|
            |_Velocity-Vector-Value23007|)
           (|_Load22997| |initial-x-speed| |_Speed-Value23009|)
           (|_Load22997| |initial-y-speed| |_Speed-Value23008|)
           (|_Load22997| |velocity| |_Velocity-Vector-Value22998|)
           (|_Load22997| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23006|)
           (|_Load22997| |x-distance| |_Length-Value23005|)
           (|_Load22997| |x-speed| |_Speed-Value23004|)
           (|_Load22997| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23003|)
           (|_Load22997| |y-distance| |_Length-Value23001|)
           (|_Load22997| |y-speed| |_Speed-Value22999|))
          NIL NIL)))
(|Living-Entity| (|_Living-Entity34009|
                  (((|_Living-Entity34009| |instance-of|
                     |Living-Entity|))
                   T NIL)))
(|Rate-Value| (|_Rate-Value37706|
               (((|_Rate-Value37706| |instance-of| |Rate-Value|)) T
                NIL)))
(|Produce| (|_Produce25573|
            (((|_Time-Interval25581| |instance-of| |Time-Interval|)
              (|_Tangible-Entity25578| |instance-of| |Tangible-Entity|)
              (|_Produce25573| |instance-of| |Produce|)
              (|_Entity25580| |instance-of| |Entity|)
              (|_Produce25573| |actions| |_Produce25573|)
              (|_Produce25573| |primitive-actions| |_Produce25573|)
              (|_Produce25573| |time-during| |_Time-Interval25581|)
              (|_Produce25573| |raw-material| |_Tangible-Entity25578|)
              (|_Produce25573| |result| |_Entity25580|))
             T NIL)))
(|KM-Slot-Group| (|_KM-Slot-Group37961|
                  (((|_KM-Slot-Group37961| |instance-of|
                     |KM-Slot-Group|))
                   T NIL)))
(|Organization| (|_Organization34822|
                 (((|_Organization34822| |instance-of| |Organization|)
                   (|_Number34824| |instance-of| |Number|)
                   (|_Organization34822| |number-of-elements|
                    |_Number34824|))
                  T NIL)))
(|Locomotion| (|_Locomotion34986|
               (((|_Time-Interval35014| |instance-of| |Time-Interval|)
                 (|_Tangible-Entity34987| |instance-of|
                  |Tangible-Entity|)
                 (|_Acceleration-Magnitude-Value34990| |instance-of|
                  |Acceleration-Magnitude-Value|)
                 (|_Acceleration-Vector-Value34989| |instance-of|
                  |Acceleration-Vector-Value|)
                 (|_Length-Value34996| |instance-of| |Length-Value|)
                 (|_Duration-Value34988| |instance-of|
                  |Duration-Value|)
                 (|_Speed-Value34992| |instance-of| |Speed-Value|)
                 (|_Displacement-Vector-Value34995| |instance-of|
                  |Displacement-Vector-Value|)
                 (|_Speed-Value35013| |instance-of| |Speed-Value|)
                 (|_Velocity-Vector-Value35010| |instance-of|
                  |Velocity-Vector-Value|)
                 (|_Speed-Value35012| |instance-of| |Speed-Value|)
                 (|_Speed-Value35011| |instance-of| |Speed-Value|)
                 (|_Speed-Value35009| |instance-of| |Speed-Value|)
                 (|_Velocity-Vector-Value35006| |instance-of|
                  |Velocity-Vector-Value|)
                 (|_Speed-Value35008| |instance-of| |Speed-Value|)
                 (|_Speed-Value35007| |instance-of| |Speed-Value|)
                 (|_Velocity-Vector-Value34991| |instance-of|
                  |Velocity-Vector-Value|)
                 (|_Acceleration-Magnitude-Value35005| |instance-of|
                  |Acceleration-Magnitude-Value|)
                 (|_Length-Value35004| |instance-of| |Length-Value|)
                 (|_Speed-Value35003| |instance-of| |Speed-Value|)
                 (|_Acceleration-Magnitude-Value35002| |instance-of|
                  |Acceleration-Magnitude-Value|)
                 (|_Length-Value35001| |instance-of| |Length-Value|)
                 (|_Locomotion34986| |instance-of| |Locomotion|)
                 (|_Speed-Value35000| |instance-of| |Speed-Value|)
                 (|_Locomotion34986| |actions| |_Locomotion34986|)
                 (|_Locomotion34986| |primitive-actions|
                  |_Locomotion34986|)
                 (|_Locomotion34986| |time-during|
                  |_Time-Interval35014|)
                 (|_Locomotion34986| |object| |_Tangible-Entity34987|)
                 (|_Locomotion34986| |agent| |_Tangible-Entity34987|)
                 (|_Locomotion34986| |acceleration-magnitude|
                  |_Acceleration-Magnitude-Value34990|)
                 (|_Locomotion34986| |acceleration|
                  |_Acceleration-Vector-Value34989|)
                 (|_Locomotion34986| |distance| |_Length-Value34996|)
                 (|_Locomotion34986| |duration| |_Duration-Value34988|)
                 (|_Locomotion34986| |speed| |_Speed-Value34992|)
                 (|_Locomotion34986| |displacement|
                  |_Displacement-Vector-Value34995|)
                 (|_Locomotion34986| |final-speed| |_Speed-Value35013|)
                 (|_Locomotion34986| |final-velocity|
                  |_Velocity-Vector-Value35010|)
                 (|_Locomotion34986| |final-x-speed|
                  |_Speed-Value35012|)
                 (|_Locomotion34986| |final-y-speed|
                  |_Speed-Value35011|)
                 (|_Locomotion34986| |initial-speed|
                  |_Speed-Value35009|)
                 (|_Locomotion34986| |initial-velocity|
                  |_Velocity-Vector-Value35006|)
                 (|_Locomotion34986| |initial-x-speed|
                  |_Speed-Value35008|)
                 (|_Locomotion34986| |initial-y-speed|
                  |_Speed-Value35007|)
                 (|_Locomotion34986| |velocity|
                  |_Velocity-Vector-Value34991|)
                 (|_Locomotion34986| |x-acceleration-magnitude|
                  |_Acceleration-Magnitude-Value35005|)
                 (|_Locomotion34986| |x-distance| |_Length-Value35004|)
                 (|_Locomotion34986| |x-speed| |_Speed-Value35003|)
                 (|_Locomotion34986| |y-acceleration-magnitude|
                  |_Acceleration-Magnitude-Value35002|)
                 (|_Locomotion34986| |y-distance| |_Length-Value35001|)
                 (|_Locomotion34986| |y-speed| |_Speed-Value35000|))
                T NIL)))
(|PH-Scale| (|_PH-Scale37487|
             (((|_PH-Scale37487| |instance-of| |PH-Scale|)
               (|_Number37489| |instance-of| |Number|)
               (|_PH-Scale37487| |number-of-elements| |_Number37489|))
              T NIL)))
(|Coefficient-of-Friction-Value| (|_Coefficient-of-Friction-Value37684|
                                  (((|_Coefficient-of-Friction-Value37684|
                                     |instance-of|
                                     |Coefficient-of-Friction-Value|))
                                   T
                                   NIL)))
(|Breakfast| (|_Breakfast715|
              (((|_Time-Interval722| |instance-of| |Time-Interval|)
                (|_Breakfast715| |instance-of| |Breakfast|)
                (|_Eat721| |instance-of| |Eat|)
                (|_Breakfast715| |actions| |_Breakfast715|)
                (|_Breakfast715| |actions| |_Eat721|)
                (|_Breakfast715| |all-subevents| |_Eat721|)
                (|_Breakfast715| |primitive-actions| |_Eat721|)
                (|_Breakfast715| |time-during| |_Time-Interval722|)
                (|_Breakfast715| |subevent| |_Eat721|))
               T NIL)))
(|Condense| (|_Condense24715|
             (((|_Time-Interval24726| |instance-of| |Time-Interval|)
               (|_Tangible-Entity24725| |instance-of|
                |Tangible-Entity|)
               (|_Density-Value24721| |instance-of| |Density-Value|)
               (|_Condense24715| |instance-of| |Condense|)
               (|_Density-Value24722| |instance-of| |Density-Value|)
               (|_Density-Value24722| |greater-than|
                |_Density-Value24721|)
               (|_Condense24715| |actions| |_Condense24715|)
               (|_Condense24715| |primitive-actions| |_Condense24715|)
               (|_Condense24715| |time-during| |_Time-Interval24726|)
               (|_Condense24715| |base| |_Tangible-Entity24725|)
               (|_Condense24715| |from-value| |_Density-Value24721|)
               (|_Condense24715| |to-value| |_Density-Value24722|))
              T NIL)))
(|Move| (|_Move10316|
         (((|_Move10344| |instance-of| |Move|)
           (|_Time-Interval10343| |instance-of| |Time-Interval|)
           (|_Tangible-Entity10342| |instance-of| |Tangible-Entity|)
           (|_Acceleration-Magnitude-Value10319| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value10318| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value10325| |instance-of| |Length-Value|)
           (|_Duration-Value10317| |instance-of| |Duration-Value|)
           (|_Speed-Value10321| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value10324| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value10341| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value10338| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value10340| |instance-of| |Speed-Value|)
           (|_Speed-Value10339| |instance-of| |Speed-Value|)
           (|_Speed-Value10337| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value10334| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value10336| |instance-of| |Speed-Value|)
           (|_Speed-Value10335| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value10320| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value10333| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value10332| |instance-of| |Length-Value|)
           (|_Speed-Value10331| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value10330| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value10329| |instance-of| |Length-Value|)
           (|_Move10316| |instance-of| |Move|)
           (|_Speed-Value10328| |instance-of| |Speed-Value|)
           (|_Move10316| |actions| |_Move10316|)
           (|_Move10316| |preparatory-event| |_Move10344|)
           (|_Move10316| |primitive-actions| |_Move10316|)
           (|_Move10316| |time-during| |_Time-Interval10343|)
           (|_Move10316| |object| |_Tangible-Entity10342|)
           (|_Move10316| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value10319|)
           (|_Move10316| |acceleration|
            |_Acceleration-Vector-Value10318|)
           (|_Move10316| |distance| |_Length-Value10325|)
           (|_Move10316| |duration| |_Duration-Value10317|)
           (|_Move10316| |speed| |_Speed-Value10321|)
           (|_Move10316| |displacement|
            |_Displacement-Vector-Value10324|)
           (|_Move10316| |final-speed| |_Speed-Value10341|)
           (|_Move10316| |final-velocity|
            |_Velocity-Vector-Value10338|)
           (|_Move10316| |final-x-speed| |_Speed-Value10340|)
           (|_Move10316| |final-y-speed| |_Speed-Value10339|)
           (|_Move10316| |initial-speed| |_Speed-Value10337|)
           (|_Move10316| |initial-velocity|
            |_Velocity-Vector-Value10334|)
           (|_Move10316| |initial-x-speed| |_Speed-Value10336|)
           (|_Move10316| |initial-y-speed| |_Speed-Value10335|)
           (|_Move10316| |velocity| |_Velocity-Vector-Value10320|)
           (|_Move10316| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value10333|)
           (|_Move10316| |x-distance| |_Length-Value10332|)
           (|_Move10316| |x-speed| |_Speed-Value10331|)
           (|_Move10316| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value10330|)
           (|_Move10316| |y-distance| |_Length-Value10329|)
           (|_Move10316| |y-speed| |_Speed-Value10328|))
          T NIL)))
(|Momentum-Value| (|_Momentum-Value37716|
                   (((|_Momentum-Value37716| |instance-of|
                      |Momentum-Value|))
                    T NIL)))
(|Make-Inaccessible| (|_Make-Inaccessible23571|
                      (((|_Time-Interval23583| |instance-of|
                         |Time-Interval|)
                        (|_Entity23579| |instance-of| |Entity|)
                        (|_Move23577| |instance-of| |Move|)
                        (|_Make-Accessible23576| |instance-of|
                         |Make-Accessible|)
                        (|_Time-Interval23575| |instance-of|
                         |Time-Interval|)
                        (|_Entity23572| |instance-of| |Spatial-Entity|)
                        (|_Make-Inaccessible23571| |instance-of|
                         |Make-Inaccessible|)
                        (|_Be-Inaccessible23574| |instance-of|
                         |Be-Inaccessible|)
                        (|_Be-Inaccessible23574| |actions|
                         |_Be-Inaccessible23574|)
                        (|_Be-Inaccessible23574| |primitive-actions|
                         |_Be-Inaccessible23574|)
                        (|_Be-Inaccessible23574| |time-during|
                         |_Time-Interval23583|)
                        (|_Be-Inaccessible23574| |object|
                         |_Entity23579|)
                        (|_Make-Inaccessible23571| |actions|
                         |_Make-Inaccessible23571|)
                        (|_Make-Inaccessible23571| |preparatory-event|
                         |_Move23577|)
                        (|_Make-Inaccessible23571| |preparatory-event|
                         |_Make-Accessible23576|)
                        (|_Make-Inaccessible23571| |primitive-actions|
                         |_Make-Inaccessible23571|)
                        (|_Make-Inaccessible23571| |time-during|
                         |_Time-Interval23575|)
                        (|_Make-Inaccessible23571| |object|
                         |_Entity23572|)
                        (|_Make-Inaccessible23571| |resulting-state|
                         |_Be-Inaccessible23574|))
                       T NIL)))
(|Device| (|_Device34202|
           (((|_Time-Interval34208| |instance-of| |Time-Interval|)
             (|_Create34205| |instance-of| |Create|)
             (|_Device34202| |instance-of| |Device|)
             (|_Instrument-Role34204| |instance-of| |Instrument-Role|)
             (|_Create34205| |actions| |_Create34205|)
             (|_Create34205| |primitive-actions| |_Create34205|)
             (|_Create34205| |time-during| |_Time-Interval34208|)
             (|_Device34202| |result-of| |_Create34205|)
             (|_Device34202| |purpose| |_Instrument-Role34204|))
            T NIL)))
(|Priority-Scale| (|_Priority-Scale37547|
                   (((|_Priority-Scale37547| |instance-of|
                      |Priority-Scale|)
                     (|_Number37549| |instance-of| |Number|)
                     (|_Priority-Scale37547| |number-of-elements|
                      |_Number37549|))
                    T NIL)))
(|Replenish| (|_Replenish2344|
              (((|_Move2349| |instance-of| |Move|)
                (|_Time-Interval2348| |instance-of| |Time-Interval|)
                (|_Resource2347| |instance-of| |Resource|)
                (|_Replenish2344| |instance-of| |Replenish|)
                (|_Entity2345| |instance-of| |Spatial-Entity|)
                (|_Entity2345| |plays| |_Resource2347|)
                (|_Replenish2344| |actions| |_Replenish2344|)
                (|_Replenish2344| |preparatory-event| |_Move2349|)
                (|_Replenish2344| |primitive-actions| |_Replenish2344|)
                (|_Replenish2344| |time-during| |_Time-Interval2348|)
                (|_Replenish2344| |base| |_Resource2347|)
                (|_Replenish2344| |object| |_Entity2345|))
               T NIL)))
(|Repair| (|_Repair9929|
           (((|_Move9937| |instance-of| |Move|)
             (|_Break9936| |instance-of| |Break|)
             (|_Time-Interval9935| |instance-of| |Time-Interval|)
             (|_Repair9929| |instance-of| |Repair|)
             (|_Tangible-Entity9933| |instance-of| |Tangible-Entity|)
             (|_Repair9929| |actions| |_Repair9929|)
             (|_Repair9929| |preparatory-event| |_Move9937|)
             (|_Repair9929| |preparatory-event| |_Break9936|)
             (|_Repair9929| |primitive-actions| |_Repair9929|)
             (|_Repair9929| |time-during| |_Time-Interval9935|)
             (|_Repair9929| |object| |_Tangible-Entity9933|))
            T NIL)))
(|Creator| (|_Creator33545|
            (((|_Create33548| |instance-of| |Create|)
              (|_Creator33545| |instance-of| |Creator|)
              (|_Tangible-Entity33547| |instance-of| |Tangible-Entity|)
              (|_Creator33545| |in-event| |_Create33548|)
              (|_Creator33545| |played-by| |_Tangible-Entity33547|))
             T NIL)))
(|Frequency-Constant| (|_Frequency-Constant37887|
                       (((|_Frequency-Constant37887| |instance-of|
                          |Frequency-Constant|))
                        T NIL)))
(|Breakability-Value| (|_Breakability-Value37754|
                       (((|_Breakability-Value37754| |instance-of|
                          |Breakability-Value|))
                        T NIL)))
(|Negation-Node| (|_Negation-Node34914|
                  (((|_Negation-Node34914| |instance-of|
                     |Negation-Node|))
                   T NIL)))
(|Situation| (|_Situation34916|
              (((|_Situation34916| |instance-of| |Situation|)) T NIL)))
(|Worth-Constant| (|_Worth-Constant37799|
                   (((|_Worth-Constant37799| |instance-of|
                      |Worth-Constant|))
                    T NIL)))
(|Platform| (|_Platform34430|
             (((|_Time-Interval34451| |instance-of| |Time-Interval|)
               (|_Time-Interval34448| |instance-of| |Time-Interval|)
               (|_Time-Interval34445| |instance-of| |Time-Interval|)
               (|_Scalar34433| |instance-of| |Scalar|)
               (|_Be-Stable34436| |instance-of| |Be-Stable|)
               (|_Be-Supported34435| |instance-of| |Be-Supported|)
               (|_Create34434| |instance-of| |Create|)
               (|_Platform34430| |instance-of| |Platform|)
               (|_Angle-Value34432| |instance-of| |Angle-Value|)
               (|_Be-Stable34436| |actions| |_Be-Stable34436|)
               (|_Be-Stable34436| |primitive-actions|
                |_Be-Stable34436|)
               (|_Be-Stable34436| |time-during| |_Time-Interval34451|)
               (|_Be-Supported34435| |actions| |_Be-Supported34435|)
               (|_Be-Supported34435| |primitive-actions|
                |_Be-Supported34435|)
               (|_Be-Supported34435| |time-during|
                |_Time-Interval34448|)
               (|_Create34434| |actions| |_Create34434|)
               (|_Create34434| |primitive-actions| |_Create34434|)
               (|_Create34434| |time-during| |_Time-Interval34445|)
               (|_Angle-Value34432| |scalar-value| |_Scalar34433|)
               (|_Platform34430| |object-of| |_Be-Stable34436|)
               (|_Platform34430| |object-of| |_Be-Supported34435|)
               (|_Platform34430| |result-of| |_Create34434|)
               (|_Platform34430| |orientation| |_Angle-Value34432|))
              T NIL)))
(|Harm| (|_Harm24772|
         (((|_Move24779| |instance-of| |Move|)
           (|_Time-Interval24778| |instance-of| |Time-Interval|)
           (|_Harm24772| |instance-of| |Harm|)
           (|_Living-Entity24776| |instance-of| |Living-Entity|)
           (|_Harm24772| |actions| |_Harm24772|)
           (|_Harm24772| |preparatory-event| |_Move24779|)
           (|_Harm24772| |primitive-actions| |_Harm24772|)
           (|_Harm24772| |time-during| |_Time-Interval24778|)
           (|_Harm24772| |object| |_Living-Entity24776|))
          T NIL)))
(|Lunch| (|_Lunch613|
          (((|_Time-Interval620| |instance-of| |Time-Interval|)
            (|_Lunch613| |instance-of| |Lunch|)
            (|_Eat619| |instance-of| |Eat|)
            (|_Lunch613| |actions| |_Lunch613|)
            (|_Lunch613| |actions| |_Eat619|)
            (|_Lunch613| |all-subevents| |_Eat619|)
            (|_Lunch613| |primitive-actions| |_Eat619|)
            (|_Lunch613| |time-during| |_Time-Interval620|)
            (|_Lunch613| |subevent| |_Eat619|))
           T NIL)))
(|Expose| (|_Expose24526|
           (((|_Move24532| |instance-of| |Move|)
             (|_Conceal24530| |instance-of| |Conceal|)
             (|_Time-Interval24529| |instance-of| |Time-Interval|)
             (|_Expose24526| |instance-of| |Expose|)
             (|_Tangible-Entity24527| |instance-of| |Tangible-Entity|)
             (|_Expose24526| |actions| |_Expose24526|)
             (|_Expose24526| |preparatory-event| |_Move24532|)
             (|_Expose24526| |preparatory-event| |_Conceal24530|)
             (|_Expose24526| |primitive-actions| |_Expose24526|)
             (|_Expose24526| |time-during| |_Time-Interval24529|)
             (|_Expose24526| |object| |_Tangible-Entity24527|))
            T NIL)))
(|Sit-Down| (|_Sit-Down10289|
             (((|_Time-Interval10302| |instance-of| |Time-Interval|)
               (|_Entity10297| |instance-of| |Entity|)
               (|_Time-Interval10296| |instance-of| |Time-Interval|)
               (|_Be-Sitting10295| |instance-of| |Be-Sitting|)
               (|_Sit-Down10289| |instance-of| |Sit-Down|)
               (|_Entity10293| |instance-of| |Tangible-Entity|)
               (|_Be-Sitting10295| |actions| |_Be-Sitting10295|)
               (|_Be-Sitting10295| |primitive-actions|
                |_Be-Sitting10295|)
               (|_Be-Sitting10295| |time-during| |_Time-Interval10302|)
               (|_Be-Sitting10295| |object| |_Entity10297|)
               (|_Sit-Down10289| |actions| |_Sit-Down10289|)
               (|_Sit-Down10289| |primitive-actions| |_Sit-Down10289|)
               (|_Sit-Down10289| |time-during| |_Time-Interval10296|)
               (|_Sit-Down10289| |object| |_Entity10293|)
               (|_Sit-Down10289| |resulting-state| |_Be-Sitting10295|)
               (|_Sit-Down10289| |agent| |_Entity10293|))
              T NIL)))
(|Glass| (|_Glass33825|
          (((|_Categorical33830| |instance-of| |Categorical|)
            (|_Glass33825| |instance-of| |Glass|)
            (|_State-Value33829| |instance-of| |State-Value|)
            (|_State-Value33829| |categorical-value|
             |_Categorical33830|)
            (|_Glass33825| |physical-state| |_State-Value33829|))
           T NIL)))
(|Product| (|_Product32985|
            (((|_Time-Interval32992| |instance-of| |Time-Interval|)
              (|_Tangible-Entity32989| |instance-of| |Tangible-Entity|)
              (|_Produce32988| |instance-of| |Produce|)
              (|_Product32985| |instance-of| |Product|)
              (|_Tangible-Entity32987| |instance-of| |Tangible-Entity|)
              (|_Produce32988| |actions| |_Produce32988|)
              (|_Produce32988| |primitive-actions| |_Produce32988|)
              (|_Produce32988| |time-during| |_Time-Interval32992|)
              (|_Produce32988| |raw-material| |_Tangible-Entity32989|)
              (|_Tangible-Entity32987| |result-of| |_Produce32988|)
              (|_Tangible-Entity32987| |material|
               |_Tangible-Entity32989|)
              (|_Product32985| |in-event| |_Produce32988|)
              (|_Product32985| |played-by| |_Tangible-Entity32987|))
             T NIL)))
(|Sentience-Value| (|_Sentience-Value37703|
                    (((|_Sentience-Value37703| |instance-of|
                       |Sentience-Value|))
                     T NIL)))
(|Specification| (|_Specification34745|
                  (((|_Language34749| |instance-of| |Language|)
                    (|_Specify34746| |instance-of| |Specify|)
                    (|_Specification34745| |instance-of|
                     |Specification|)
                    (|_Thing34748| |instance-of| |Thing|)
                    (|_Specification34745| |information-language|
                     |_Language34749|)
                    (|_Specification34745| |result-of| |_Specify34746|)
                    (|_Specification34745| |information-content|
                     |_Thing34748|))
                   T NIL)))
(|Pressure-Constant| (|_Pressure-Constant37851|
                      (((|_Pressure-Constant37851| |instance-of|
                         |Pressure-Constant|))
                       T NIL)))
(|Text-Field| (|_Text-Field34768|
               (((|_Language34770| |instance-of| |Language|)
                 (|_Text-Field34768| |instance-of| |Text-Field|)
                 (|_String34769| |instance-of| |String|)
                 (|_Text-Field34768| |information-language|
                  |_Language34770|)
                 (|_Text-Field34768| |information-content|
                  |_String34769|))
                T NIL)))
(|Institution| (|_Institution34827|
                (((|_Institution34827| |instance-of| |Institution|)
                  (|_Number34829| |instance-of| |Number|)
                  (|_Institution34827| |number-of-elements|
                   |_Number34829|))
                 T NIL)))
(|UoM-Acceleration-Magnitude| (|_UoM-Acceleration-Magnitude37478|
                               (((|_UoM-Acceleration-Magnitude37478|
                                  |instance-of|
                                  |UoM-Acceleration-Magnitude|))
                                T
                                NIL)))
(|Class| (|_Class37979|
          (((|_Class37979| |instance-of| |Class|)) T NIL)))
(|Collide| (|_Collide24105|
            (((|_Time-Interval24120| |instance-of| |Time-Interval|)
              (|_Move24115| |instance-of| |Move|)
              (|_Break-Contact24114| |instance-of| |Break-Contact|)
              (|_Time-Interval24113| |instance-of| |Time-Interval|)
              (|_Tangible-Entity24112| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity24109| |instance-of| |Tangible-Entity|)
              (|_Be-Touching24111| |instance-of| |Be-Touching|)
              (|_Collide24105| |instance-of| |Collide|)
              (|_Be-Touching24111| |actions| |_Be-Touching24111|)
              (|_Be-Touching24111| |primitive-actions|
               |_Be-Touching24111|)
              (|_Be-Touching24111| |time-during| |_Time-Interval24120|)
              (|_Collide24105| |actions| |_Collide24105|)
              (|_Collide24105| |preparatory-event| |_Move24115|)
              (|_Collide24105| |preparatory-event|
               |_Break-Contact24114|)
              (|_Collide24105| |primitive-actions| |_Collide24105|)
              (|_Collide24105| |time-during| |_Time-Interval24113|)
              (|_Collide24105| |base| |_Tangible-Entity24112|)
              (|_Collide24105| |object| |_Tangible-Entity24109|)
              (|_Collide24105| |resulting-state| |_Be-Touching24111|)
              (|_Collide24105| |intensity| |*strong|))
             T NIL)))
(|Training| (|_Training424|
             (((|_Training424| |instance-of| |Training|)
               (|_Time-Interval430| |instance-of| |Time-Interval|)
               (|_Training424| |actions| |_Training424|)
               (|_Training424| |primitive-actions| |_Training424|)
               (|_Training424| |time-during| |_Time-Interval430|))
              T NIL)))
(|Research| (|_Research471|
             (((|_Research471| |instance-of| |Research|)
               (|_Time-Interval477| |instance-of| |Time-Interval|)
               (|_Research471| |actions| |_Research471|)
               (|_Research471| |primitive-actions| |_Research471|)
               (|_Research471| |time-during| |_Time-Interval477|))
              T NIL)))
(|Spatial-Entity| (|_Spatial-Entity33603|
                   (((|_Spatial-Entity33603| |instance-of|
                      |Spatial-Entity|))
                    T NIL)))
(|Lie| (|_Lie25094|
        (((|_Move25105| |instance-of| |Move|)
          (|_Time-Interval25104| |instance-of| |Time-Interval|)
          (|_Information25102| |instance-of| |Information|)
          (|_Information25102| |instance-of| |Spatial-Entity|)
          (|_Message25103| |instance-of| |Message|)
          (|_Lie25094| |instance-of| |Lie|)
          (|_Tangible-Entity25100| |instance-of| |Tangible-Entity|)
          (|_Lie25094| |actions| |_Lie25094|)
          (|_Lie25094| |preparatory-event| |_Move25105|)
          (|_Lie25094| |primitive-actions| |_Lie25094|)
          (|_Lie25094| |time-during| |_Time-Interval25104|)
          (|_Lie25094| |object| |_Information25102|)
          (|_Lie25094| |result| |_Message25103|)
          (|_Lie25094| |agent| |_Tangible-Entity25100|))
         T NIL)))
(|Take| (|_Take1067|
         (((|_Move1086| |instance-of| |Move|)
           (|_Obtain1085| |instance-of| |Obtain|)
           (|_Time-Interval1084| |instance-of| |Time-Interval|)
           (|_Move1079| |instance-of| |Move|)
           (|_Obtain1078| |instance-of| |Obtain|)
           (|_Time-Interval1077| |instance-of| |Time-Interval|)
           (|_Entity1076| |instance-of| |Spatial-Entity|)
           (|_Entity1073| |instance-of| |Tangible-Entity|)
           (|_Take1067| |instance-of| |Take|)
           (|_Tangible-Entity1074| |instance-of| |Tangible-Entity|)
           (|_Obtain1078| |actions| |_Obtain1078|)
           (|_Obtain1078| |preparatory-event| |_Move1086|)
           (|_Obtain1078| |preparatory-event| |_Obtain1085|)
           (|_Obtain1078| |primitive-actions| |_Obtain1078|)
           (|_Obtain1078| |time-during| |_Time-Interval1084|)
           (|_Obtain1078| |object| |_Entity1076|)
           (|_Obtain1078| |recipient| |_Tangible-Entity1074|)
           (|_Obtain1078| |agent| |_Tangible-Entity1074|)
           (|_Take1067| |actions| |_Take1067|)
           (|_Take1067| |preparatory-event| |_Move1079|)
           (|_Take1067| |preparatory-event| |_Obtain1078|)
           (|_Take1067| |primitive-actions| |_Take1067|)
           (|_Take1067| |time-during| |_Time-Interval1077|)
           (|_Take1067| |object| |_Entity1076|)
           (|_Take1067| |recipient| |_Entity1073|)
           (|_Take1067| |agent| |_Entity1073|)
           (|_Take1067| |donor| |_Tangible-Entity1074|))
          T NIL)))
(|Embody| (|_Embody25584|
           (((|_Move25593| |instance-of| |Move|)
             (|_Time-Interval25592| |instance-of| |Time-Interval|)
             (|_Message25589| |instance-of| |Message|)
             (|_Message25589| |instance-of| |Spatial-Entity|)
             (|_Embody25584| |instance-of| |Embody|)
             (|_Tangible-Entity25591| |instance-of| |Tangible-Entity|)
             (|_Embody25584| |actions| |_Embody25584|)
             (|_Embody25584| |preparatory-event| |_Move25593|)
             (|_Embody25584| |primitive-actions| |_Embody25584|)
             (|_Embody25584| |time-during| |_Time-Interval25592|)
             (|_Embody25584| |object| |_Message25589|)
             (|_Embody25584| |result| |_Tangible-Entity25591|))
            T NIL)))
(|Luminous-Intensity-Value| (|_Luminous-Intensity-Value37733|
                             (((|_Luminous-Intensity-Value37733|
                                |instance-of|
                                |Luminous-Intensity-Value|))
                              T NIL)))
(|Texture-Constant| (|_Texture-Constant37819|
                     (((|_Texture-Constant37819| |instance-of|
                        |Texture-Constant|))
                      T NIL)))
(|Rotational-Rate-Constant| (|_Rotational-Rate-Constant37839|
                             (((|_Rotational-Rate-Constant37839|
                                |instance-of|
                                |Rotational-Rate-Constant|))
                              T NIL)))
(|Radius-Constant| (|_Radius-Constant37843|
                    (((|_Radius-Constant37843| |instance-of|
                       |Radius-Constant|))
                     T NIL)))
(|Administrative-District| (|_Administrative-District34677|
                            (((|_Administrative-District34677|
                               |instance-of|
                               |Administrative-District|))
                             T NIL)))
(|Cliche| (|_Cliche34980|
           (((|_Cliche34980| |instance-of| |Cliche|)) T NIL)))
(|Stone| (|_Stone33701|
          (((|_Categorical33706| |instance-of| |Categorical|)
            (|_Stone33701| |instance-of| |Stone|)
            (|_State-Value33705| |instance-of| |State-Value|)
            (|_State-Value33705| |categorical-value|
             |_Categorical33706|)
            (|_Stone33701| |physical-state| |_State-Value33705|))
           T NIL)))
(|Force-Value| (|_Force-Value37761|
                (((|_Force-Value37761| |instance-of| |Force-Value|)) T
                 NIL)))
(|Coefficient-of-Friction-Constant| (|_Coefficient-of-Friction-Constant37925|
                                     (((|_Coefficient-of-Friction-Constant37925|
                                        |instance-of|
                                        |Coefficient-of-Friction-Constant|))
                                      T
                                      NIL)))
(|Deliver| (|_Deliver1852|
            (((|_Move1862| |instance-of| |Move|)
              (|_Obtain1861| |instance-of| |Obtain|)
              (|_Time-Interval1860| |instance-of| |Time-Interval|)
              (|_Entity1859| |instance-of| |Spatial-Entity|)
              (|_Deliver1852| |instance-of| |Deliver|)
              (|_Tangible-Entity1857| |instance-of| |Tangible-Entity|)
              (|_Deliver1852| |actions| |_Deliver1852|)
              (|_Deliver1852| |preparatory-event| |_Move1862|)
              (|_Deliver1852| |preparatory-event| |_Obtain1861|)
              (|_Deliver1852| |primitive-actions| |_Deliver1852|)
              (|_Deliver1852| |time-during| |_Time-Interval1860|)
              (|_Deliver1852| |object| |_Entity1859|)
              (|_Deliver1852| |recipient| |_Tangible-Entity1857|))
             T NIL)))
(|Restrain| (|_Restrain23747|
             (((|_Time-Interval23763| |instance-of| |Time-Interval|)
               (|_Entity23759| |instance-of| |Tangible-Entity|)
               (|_Move23757| |instance-of| |Move|)
               (|_Unrestrain23755| |instance-of| |Unrestrain|)
               (|_Time-Interval23754| |instance-of| |Time-Interval|)
               (|_Tangible-Entity23751| |instance-of|
                |Tangible-Entity|)
               (|_Restrain23747| |instance-of| |Restrain|)
               (|_Be-Restrained23753| |instance-of| |Be-Restrained|)
               (|_Be-Restrained23753| |actions| |_Be-Restrained23753|)
               (|_Be-Restrained23753| |primitive-actions|
                |_Be-Restrained23753|)
               (|_Be-Restrained23753| |time-during|
                |_Time-Interval23763|)
               (|_Be-Restrained23753| |object| |_Entity23759|)
               (|_Restrain23747| |actions| |_Restrain23747|)
               (|_Restrain23747| |preparatory-event| |_Move23757|)
               (|_Restrain23747| |preparatory-event|
                |_Unrestrain23755|)
               (|_Restrain23747| |primitive-actions| |_Restrain23747|)
               (|_Restrain23747| |time-during| |_Time-Interval23754|)
               (|_Restrain23747| |object| |_Tangible-Entity23751|)
               (|_Restrain23747| |resulting-state|
                |_Be-Restrained23753|))
              T NIL)))
(|Head-Nod| (|_Head-Nod25759|
             (((|_Agent-Role25774| |instance-of| |Agent-Role|)
               (|_Move25773| |instance-of| |Move|)
               (|_Time-Interval25772| |instance-of| |Time-Interval|)
               (|_Message25769| |instance-of| |Message|)
               (|_Message25769| |instance-of| |Tangible-Entity|)
               (|_Light25770| |instance-of| |Light|)
               (|_Animal25766| |instance-of| |Animal|)
               (|_Head-Nod25759| |instance-of| |Head-Nod|)
               (|_Tangible-Entity25767| |instance-of|
                |Tangible-Entity|)
               (|_Animal25766| |capability| |_Agent-Role25774|)
               (|_Tangible-Entity25767| |abuts| |_Animal25766|)
               (|_Head-Nod25759| |actions| |_Head-Nod25759|)
               (|_Head-Nod25759| |preparatory-event| |_Move25773|)
               (|_Head-Nod25759| |primitive-actions| |_Head-Nod25759|)
               (|_Head-Nod25759| |time-during| |_Time-Interval25772|)
               (|_Head-Nod25759| |object| |_Message25769|)
               (|_Head-Nod25759| |result| |_Light25770|)
               (|_Head-Nod25759| |agent| |_Animal25766|)
               (|_Head-Nod25759| |instrument| |_Tangible-Entity25767|))
              T NIL)))
(|Scale| (|_Scale37482|
          (((|_Scale37482| |instance-of| |Scale|)
            (|_Number37484| |instance-of| |Number|)
            (|_Scale37482| |number-of-elements| |_Number37484|))
           T NIL)))
(|Distance-Scale| (|_Distance-Scale37602|
                   (((|_Distance-Scale37602| |instance-of|
                      |Distance-Scale|)
                     (|_Number37604| |instance-of| |Number|)
                     (|_Distance-Scale37602| |number-of-elements|
                      |_Number37604|))
                    T NIL)))
(|System| (|_System33592|
           (((|_System33592| |instance-of| |System|)) T NIL)))
(|Brightness-Constant| (|_Brightness-Constant37911|
                        (((|_Brightness-Constant37911| |instance-of|
                           |Brightness-Constant|))
                         T NIL)))
(|Make-Contact| (|_Make-Contact24073|
                 (((|_Time-Interval24085| |instance-of|
                    |Time-Interval|)
                   (|_Move24080| |instance-of| |Move|)
                   (|_Break-Contact24079| |instance-of|
                    |Break-Contact|)
                   (|_Time-Interval24078| |instance-of|
                    |Time-Interval|)
                   (|_Tangible-Entity24077| |instance-of|
                    |Tangible-Entity|)
                   (|_Tangible-Entity24074| |instance-of|
                    |Tangible-Entity|)
                   (|_Make-Contact24073| |instance-of| |Make-Contact|)
                   (|_Be-Touching24076| |instance-of| |Be-Touching|)
                   (|_Be-Touching24076| |actions| |_Be-Touching24076|)
                   (|_Be-Touching24076| |primitive-actions|
                    |_Be-Touching24076|)
                   (|_Be-Touching24076| |time-during|
                    |_Time-Interval24085|)
                   (|_Make-Contact24073| |actions|
                    |_Make-Contact24073|)
                   (|_Make-Contact24073| |preparatory-event|
                    |_Move24080|)
                   (|_Make-Contact24073| |preparatory-event|
                    |_Break-Contact24079|)
                   (|_Make-Contact24073| |primitive-actions|
                    |_Make-Contact24073|)
                   (|_Make-Contact24073| |time-during|
                    |_Time-Interval24078|)
                   (|_Make-Contact24073| |base|
                    |_Tangible-Entity24077|)
                   (|_Make-Contact24073| |object|
                    |_Tangible-Entity24074|)
                   (|_Make-Contact24073| |resulting-state|
                    |_Be-Touching24076|))
                  T NIL)))
(|Mass-Value| (|_Mass-Value37720|
               (((|_Mass-Value37720| |instance-of| |Mass-Value|)) T
                NIL)))
(|Company| (|_Company34842|
            (((|_Company34842| |instance-of| |Company|)
              (|_Number34844| |instance-of| |Number|)
              (|_Company34842| |number-of-elements| |_Number34844|))
             T NIL)))
(|Molecule| (|_Molecule33986|
             (((|_Molecule33986| |instance-of| |Molecule|)
               (|_Atom33988| |instance-of| |Atom|)
               (|_Molecule33986| |has-basic-structural-unit|
                |_Atom33988|))
              T NIL)))
(|Pulley| (|_Pulley34621|
           (((|_Pulley34621| |instance-of| |Pulley|)) T NIL)))
(|Distinguishing| (|_Distinguishing875|
                   (((|_Distinguishing875| |instance-of|
                      |Distinguishing|)
                     (|_Time-Interval881| |instance-of|
                      |Time-Interval|)
                     (|_Distinguishing875| |actions|
                      |_Distinguishing875|)
                     (|_Distinguishing875| |primitive-actions|
                      |_Distinguishing875|)
                     (|_Distinguishing875| |time-during|
                      |_Time-Interval881|))
                    T NIL)))
(|Fuel| (|_Fuel32971|
         (((|_Fuel32971| |instance-of| |Fuel|)
           (|_Consume32973| |instance-of| |Consume|)
           (|_Fuel32971| |in-event| |_Consume32973|))
          T NIL)))
(|Piece-of-Stone| (|_Piece-of-Stone34067|
                   (((|_Piece-of-Stone34067| |instance-of|
                      |Piece-of-Stone|)
                     (|_Stone34070| |instance-of| |Stone|)
                     (|_Piece-of-Stone34067| |material| |_Stone34070|))
                    T NIL)))
(|Propel| (|_Propel10797|
           (((|_Target10799| |instance-of| |Target|)
             (|_Move10827| |instance-of| |Move|)
             (|_Time-Interval10826| |instance-of| |Time-Interval|)
             (|_Spatial-Entity10798| |instance-of| |Spatial-Entity|)
             (|_Tangible-Entity10811| |instance-of| |Tangible-Entity|)
             (|_Acceleration-Magnitude-Value10802| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Acceleration-Vector-Value10801| |instance-of|
              |Acceleration-Vector-Value|)
             (|_Length-Value10808| |instance-of| |Length-Value|)
             (|_Duration-Value10800| |instance-of| |Duration-Value|)
             (|_Speed-Value10804| |instance-of| |Speed-Value|)
             (|_Displacement-Vector-Value10807| |instance-of|
              |Displacement-Vector-Value|)
             (|_Speed-Value10825| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value10822| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value10824| |instance-of| |Speed-Value|)
             (|_Speed-Value10823| |instance-of| |Speed-Value|)
             (|_Speed-Value10821| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value10818| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value10820| |instance-of| |Speed-Value|)
             (|_Speed-Value10819| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value10803| |instance-of|
              |Velocity-Vector-Value|)
             (|_Acceleration-Magnitude-Value10817| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value10816| |instance-of| |Length-Value|)
             (|_Speed-Value10815| |instance-of| |Speed-Value|)
             (|_Acceleration-Magnitude-Value10814| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value10813| |instance-of| |Length-Value|)
             (|_Propel10797| |instance-of| |Propel|)
             (|_Speed-Value10812| |instance-of| |Speed-Value|)
             (|_Spatial-Entity10798| |plays| |_Target10799|)
             (|_Propel10797| |actions| |_Propel10797|)
             (|_Propel10797| |preparatory-event| |_Move10827|)
             (|_Propel10797| |primitive-actions| |_Propel10797|)
             (|_Propel10797| |time-during| |_Time-Interval10826|)
             (|_Propel10797| |destination| |_Spatial-Entity10798|)
             (|_Propel10797| |toward| |_Spatial-Entity10798|)
             (|_Propel10797| |object| |_Tangible-Entity10811|)
             (|_Propel10797| |acceleration-magnitude|
              |_Acceleration-Magnitude-Value10802|)
             (|_Propel10797| |acceleration|
              |_Acceleration-Vector-Value10801|)
             (|_Propel10797| |distance| |_Length-Value10808|)
             (|_Propel10797| |duration| |_Duration-Value10800|)
             (|_Propel10797| |speed| |_Speed-Value10804|)
             (|_Propel10797| |displacement|
              |_Displacement-Vector-Value10807|)
             (|_Propel10797| |final-speed| |_Speed-Value10825|)
             (|_Propel10797| |final-velocity|
              |_Velocity-Vector-Value10822|)
             (|_Propel10797| |final-x-speed| |_Speed-Value10824|)
             (|_Propel10797| |final-y-speed| |_Speed-Value10823|)
             (|_Propel10797| |initial-speed| |_Speed-Value10821|)
             (|_Propel10797| |initial-velocity|
              |_Velocity-Vector-Value10818|)
             (|_Propel10797| |initial-x-speed| |_Speed-Value10820|)
             (|_Propel10797| |initial-y-speed| |_Speed-Value10819|)
             (|_Propel10797| |velocity| |_Velocity-Vector-Value10803|)
             (|_Propel10797| |x-acceleration-magnitude|
              |_Acceleration-Magnitude-Value10817|)
             (|_Propel10797| |x-distance| |_Length-Value10816|)
             (|_Propel10797| |x-speed| |_Speed-Value10815|)
             (|_Propel10797| |y-acceleration-magnitude|
              |_Acceleration-Magnitude-Value10814|)
             (|_Propel10797| |y-distance| |_Length-Value10813|)
             (|_Propel10797| |y-speed| |_Speed-Value10812|))
            T NIL)))
(|Frequency-Value| (|_Frequency-Value37743|
                    (((|_Frequency-Value37743| |instance-of|
                       |Frequency-Value|))
                     T NIL)))
(|Conduit| (|_Conduit33038|
            (((|_Move33041| |instance-of| |Move|)
              (|_Conduit33038| |instance-of| |Conduit|)
              (|_Spatial-Entity33040| |instance-of| |Spatial-Entity|)
              (|_Conduit33038| |in-event| |_Move33041|)
              (|_Conduit33038| |played-by| |_Spatial-Entity33040|))
             T NIL)))
(|Participant| (|_Participant33542|
                (((|_Participant33542| |instance-of| |Participant|)
                  (|_Entity33544| |instance-of| |Entity|)
                  (|_Participant33542| |played-by| |_Entity33544|))
                 T NIL)))
(|Velocity-Constant| (|_Velocity-Constant37809|
                      (((|_Velocity-Constant37809| |instance-of|
                         |Velocity-Constant|))
                       T NIL)))
(|Acceleration-Vector-Value| (|_Acceleration-Vector-Value37767|
                              (((|_Acceleration-Magnitude-Value37768|
                                 |instance-of|
                                 |Acceleration-Magnitude-Value|)
                                (|_Angle-Value37769|
                                 |instance-of|
                                 |Angle-Value|)
                                (|_Acceleration-Magnitude-Value37771|
                                 |instance-of|
                                 |Acceleration-Magnitude-Value|)
                                (|_Acceleration-Vector-Value37767|
                                 |instance-of|
                                 |Acceleration-Vector-Value|)
                                (|_Acceleration-Magnitude-Value37770|
                                 |instance-of|
                                 |Acceleration-Magnitude-Value|)
                                (|_Acceleration-Vector-Value37767|
                                 |x-component-slot|
                                 |x-acceleration-magnitude|)
                                (|_Acceleration-Vector-Value37767|
                                 |y-component-slot|
                                 |y-acceleration-magnitude|)
                                (|_Acceleration-Vector-Value37767|
                                 |acceleration-magnitude|
                                 |_Acceleration-Magnitude-Value37768|)
                                (|_Acceleration-Vector-Value37767|
                                 |direction|
                                 |_Angle-Value37769|)
                                (|_Acceleration-Vector-Value37767|
                                 |x-acceleration-magnitude|
                                 |_Acceleration-Magnitude-Value37771|)
                                (|_Acceleration-Vector-Value37767|
                                 |y-acceleration-magnitude|
                                 |_Acceleration-Magnitude-Value37770|))
                               T NIL)))
(|UoM-Luminance| (|_UoM-Luminance37458|
                  (((|_UoM-Luminance37458| |instance-of|
                     |UoM-Luminance|))
                   T NIL)))
(|Template| (|_Template32932|
             (((|_Copy32935| |instance-of| |Copy|)
               (|_Template32932| |instance-of| |Template|)
               (|_Tangible-Entity32934| |instance-of|
                |Tangible-Entity|)
               (|_Template32932| |in-event| |_Copy32935|)
               (|_Template32932| |played-by| |_Tangible-Entity32934|))
              T NIL)))
(|Be-Ruined| (|_Be-Ruined350|
              (((|_Time-Interval354| |instance-of| |Time-Interval|)
                (|_Be-Ruined350| |instance-of| |Be-Ruined|)
                (|_Tangible-Entity353| |instance-of| |Physical-Object|)
                (|_Be-Ruined350| |actions| |_Be-Ruined350|)
                (|_Be-Ruined350| |primitive-actions| |_Be-Ruined350|)
                (|_Be-Ruined350| |time-during| |_Time-Interval354|)
                (|_Be-Ruined350| |object| |_Tangible-Entity353|))
               T NIL)))
(|Luminous-Flux-Value| (|_Luminous-Flux-Value37735|
                        (((|_Luminous-Flux-Value37735| |instance-of|
                           |Luminous-Flux-Value|))
                         T NIL)))
(|Collecting| (|_Collecting947|
               (((|_Collecting947| |instance-of| |Collecting|)
                 (|_Time-Interval953| |instance-of| |Time-Interval|)
                 (|_Collecting947| |actions| |_Collecting947|)
                 (|_Collecting947| |primitive-actions|
                  |_Collecting947|)
                 (|_Collecting947| |time-during| |_Time-Interval953|))
                T NIL)))
(|Be-Supported| (|_Be-Supported264|
                 (((|_Time-Interval268| |instance-of| |Time-Interval|)
                   (|_Be-Supported264| |instance-of| |Be-Supported|)
                   (|_Entity267| |instance-of| |Entity|)
                   (|_Be-Supported264| |actions| |_Be-Supported264|)
                   (|_Be-Supported264| |primitive-actions|
                    |_Be-Supported264|)
                   (|_Be-Supported264| |time-during|
                    |_Time-Interval268|)
                   (|_Be-Supported264| |object| |_Entity267|))
                  T NIL)))
(|Be-Controlled| (|_Be-Controlled357|
                  (((|_Time-Interval361| |instance-of| |Time-Interval|)
                    (|_Be-Controlled357| |instance-of| |Be-Controlled|)
                    (|_Entity360| |instance-of| |Entity|)
                    (|_Be-Controlled357| |actions| |_Be-Controlled357|)
                    (|_Be-Controlled357| |primitive-actions|
                     |_Be-Controlled357|)
                    (|_Be-Controlled357| |time-during|
                     |_Time-Interval361|)
                    (|_Be-Controlled357| |object| |_Entity360|))
                   T NIL)))
(|Duration-Constant| (|_Duration-Constant37889|
                      (((|_Duration-Constant37889| |instance-of|
                         |Duration-Constant|))
                       T NIL)))
(|UoM-Time| (|_UoM-Time37432|
             (((|_UoM-Time37432| |instance-of| |UoM-Time|)) T NIL)))
(|Vehicle| (|_Vehicle32914|
            (((|_Carry32917| |instance-of| |Carry|)
              (|_Vehicle32914| |instance-of| |Vehicle|)
              (|_Tangible-Entity32916| |instance-of| |Tangible-Entity|)
              (|_Vehicle32914| |in-event| |_Carry32917|)
              (|_Vehicle32914| |played-by| |_Tangible-Entity32916|))
             T NIL)))
(|Angle-Value| (|_Angle-Value37760|
                (((|_Angle-Value37760| |instance-of| |Angle-Value|)) T
                 NIL)))
(|Causal-Relation| (|_Causal-Relation37949|
                    (((|_Causal-Relation37949| |instance-of|
                       |Causal-Relation|))
                     T NIL)))
(|Capacity-Scale| (|_Capacity-Scale37632|
                   (((|_Capacity-Scale37632| |instance-of|
                      |Capacity-Scale|)
                     (|_Number37634| |instance-of| |Number|)
                     (|_Capacity-Scale37632| |number-of-elements|
                      |_Number37634|))
                    T NIL)))
(|SHAKEN-Table-Header-Column| (|_SHAKEN-Table-Header-Column33596|
                               (((|_SHAKEN-Table-Header-Column33596|
                                  |instance-of|
                                  |SHAKEN-Table-Header-Column|))
                                T
                                NIL)))
(|Density-Scale| (|_Density-Scale37622|
                  (((|_Density-Scale37622| |instance-of|
                     |Density-Scale|)
                    (|_Number37624| |instance-of| |Number|)
                    (|_Density-Scale37622| |number-of-elements|
                     |_Number37624|))
                   T NIL)))
(|Information-Sequence| (|_Information-Sequence34807|
                         (((|_Information-Sequence34807| |instance-of|
                            |Information-Sequence|)
                           (|_Thing34809| |instance-of| |Thing|)
                           (|_Information-Sequence34807|
                            |information-content| |_Thing34809|))
                          T NIL)))
(|Role| (|_Role32904| (((|_Role32904| |instance-of| |Role|)) T NIL)))
(|Aggregate| (|_Aggregate34812|
              (((|_Aggregate34812| |instance-of| |Aggregate|)
                (|_Number34814| |instance-of| |Number|)
                (|_Aggregate34812| |number-of-elements|
                 |_Number34814|))
               T NIL)))
(|Attach| (|_Attach32801|
           (((|_Time-Interval32813| |instance-of| |Time-Interval|)
             (|_Move32808| |instance-of| |Move|)
             (|_Make-Contact32807| |instance-of| |Make-Contact|)
             (|_Time-Interval32806| |instance-of| |Time-Interval|)
             (|_Tangible-Entity32805| |instance-of| |Tangible-Entity|)
             (|_Tangible-Entity32802| |instance-of| |Tangible-Entity|)
             (|_Attach32801| |instance-of| |Attach|)
             (|_Be-Attached-To32804| |instance-of| |Be-Attached-To|)
             (|_Be-Attached-To32804| |actions| |_Be-Attached-To32804|)
             (|_Be-Attached-To32804| |primitive-actions|
              |_Be-Attached-To32804|)
             (|_Be-Attached-To32804| |time-during|
              |_Time-Interval32813|)
             (|_Attach32801| |actions| |_Attach32801|)
             (|_Attach32801| |preparatory-event| |_Move32808|)
             (|_Attach32801| |preparatory-event| |_Make-Contact32807|)
             (|_Attach32801| |primitive-actions| |_Attach32801|)
             (|_Attach32801| |time-during| |_Time-Interval32806|)
             (|_Attach32801| |base| |_Tangible-Entity32805|)
             (|_Attach32801| |object| |_Tangible-Entity32802|)
             (|_Attach32801| |resulting-state| |_Be-Attached-To32804|))
            T NIL)))
(|Buy| (|_Buy1116|
        (((|_Move1200| |instance-of| |Move|)
          (|_Obtain1199| |instance-of| |Obtain|)
          (|_Time-Interval1198| |instance-of| |Time-Interval|)
          (|_Entity1195| |instance-of| |Tangible-Entity|)
          (|_Move1190| |instance-of| |Move|)
          (|_Obtain1189| |instance-of| |Obtain|)
          (|_Time-Interval1188| |instance-of| |Time-Interval|)
          (|_Move1181| |instance-of| |Move|)
          (|_Obtain1180| |instance-of| |Obtain|)
          (|_Time-Interval1179| |instance-of| |Time-Interval|)
          (|_Move1143| |instance-of| |Move|)
          (|_Obtain1142| |instance-of| |Obtain|)
          (|_Time-Interval1141| |instance-of| |Time-Interval|)
          (|_Resource1135| |instance-of| |Resource|)
          (|_Resource1134| |instance-of| |Resource|)
          (|_Worth-Value1133| |instance-of| |Worth-Value|)
          (|_Move1131| |instance-of| |Move|)
          (|_Obtain1130| |instance-of| |Obtain|)
          (|_Time-Interval1129| |instance-of| |Time-Interval|)
          (|_Entity1127| |instance-of| |Spatial-Entity|)
          (|_Transfer1128| |instance-of| |Transfer|)
          (|_Entity1123| |instance-of| |Tangible-Entity|)
          (|_Tangible-Entity1124| |instance-of| |Tangible-Entity|)
          (|_Buy1116| |instance-of| |Buy|)
          (|_Money1125| |instance-of| |Money|)
          (|_Money1125| |instance-of| |Spatial-Entity|)
          (|_Obtain1189| |actions| |_Obtain1189|)
          (|_Obtain1189| |preparatory-event| |_Move1200|)
          (|_Obtain1189| |preparatory-event| |_Obtain1199|)
          (|_Obtain1189| |primitive-actions| |_Obtain1189|)
          (|_Obtain1189| |time-during| |_Time-Interval1198|)
          (|_Obtain1189| |object| |_Money1125|)
          (|_Obtain1189| |recipient| |_Entity1195|)
          (|_Obtain1189| |agent| |_Entity1195|)
          (|_Obtain1180| |actions| |_Obtain1180|)
          (|_Obtain1180| |preparatory-event| |_Move1190|)
          (|_Obtain1180| |preparatory-event| |_Obtain1189|)
          (|_Obtain1180| |primitive-actions| |_Obtain1180|)
          (|_Obtain1180| |time-during| |_Time-Interval1188|)
          (|_Obtain1180| |object| |_Money1125|)
          (|_Obtain1180| |recipient| |_Entity1123|)
          (|_Obtain1180| |agent| |_Entity1123|)
          (|_Entity1123| |abuts| |_Money1125|)
          (|_Entity1123| |object-of| |_Move1131|)
          (|_Transfer1128| |actions-of| |_Transfer1128|)
          (|_Transfer1128| |preparatory-event| |_Move1181|)
          (|_Transfer1128| |preparatory-event| |_Obtain1180|)
          (|_Transfer1128| |primitive-actions-of| |_Transfer1128|)
          (|_Transfer1128| |time-during| |_Time-Interval1179|)
          (|_Transfer1128| |object| |_Money1125|)
          (|_Transfer1128| |recipient| |_Tangible-Entity1124|)
          (|_Transfer1128| |agent| |_Entity1123|)
          (|_Transfer1128| |donor| |_Entity1123|)
          (|_Obtain1130| |actions| |_Obtain1130|)
          (|_Obtain1130| |preparatory-event| |_Move1143|)
          (|_Obtain1130| |preparatory-event| |_Obtain1142|)
          (|_Obtain1130| |primitive-actions| |_Obtain1130|)
          (|_Obtain1130| |time-during| |_Time-Interval1141|)
          (|_Obtain1130| |object| |_Entity1127|)
          (|_Obtain1130| |recipient| |_Tangible-Entity1124|)
          (|_Obtain1130| |agent| |_Tangible-Entity1124|)
          (|_Money1125| |purpose| |_Resource1135|)
          (|_Money1125| |purpose| |_Resource1134|)
          (|_Money1125| |worth| |_Worth-Value1133|)
          (|_Buy1116| |actions| |_Buy1116|)
          (|_Buy1116| |actions| |_Transfer1128|)
          (|_Buy1116| |all-subevents| |_Transfer1128|)
          (|_Buy1116| |preparatory-event| |_Move1131|)
          (|_Buy1116| |preparatory-event| |_Obtain1130|)
          (|_Buy1116| |primitive-actions| |_Transfer1128|)
          (|_Buy1116| |time-during| |_Time-Interval1129|)
          (|_Buy1116| |object| |_Entity1127|)
          (|_Buy1116| |recipient| |_Entity1123|)
          (|_Buy1116| |first-subevent| |_Transfer1128|)
          (|_Buy1116| |subevent| |_Transfer1128|)
          (|_Buy1116| |agent| |_Entity1123|)
          (|_Buy1116| |donor| |_Tangible-Entity1124|)
          (|_Buy1116| |instrument| |_Money1125|))
         T NIL)))
(|Filter| (|_Filter33028|
           (((|_Filter33028| |instance-of| |Filter|)
             (|_Entity33030| |instance-of| |Entity|)
             (|_Filter33028| |played-by| |_Entity33030|))
            T NIL)))
(|Pressure-Value| (|_Pressure-Value37710|
                   (((|_Pressure-Value37710| |instance-of|
                      |Pressure-Value|))
                    T NIL)))
(|UoM-Voltage| (|_UoM-Voltage37428|
                (((|_UoM-Voltage37428| |instance-of| |UoM-Voltage|)) T
                 NIL)))
(|Add| (|_Add32856|
        (((|_Move32865| |instance-of| |Move|)
          (|_Move32864| |instance-of| |Move|)
          (|_Time-Interval32863| |instance-of| |Time-Interval|)
          (|_Tangible-Entity32862| |instance-of| |Tangible-Entity|)
          (|_Add32856| |instance-of| |Add|)
          (|_Tangible-Entity32860| |instance-of| |Tangible-Entity|)
          (|_Add32856| |actions| |_Add32856|)
          (|_Add32856| |preparatory-event| |_Move32865|)
          (|_Add32856| |preparatory-event| |_Move32864|)
          (|_Add32856| |primitive-actions| |_Add32856|)
          (|_Add32856| |time-during| |_Time-Interval32863|)
          (|_Add32856| |base| |_Tangible-Entity32862|)
          (|_Add32856| |object| |_Tangible-Entity32860|))
         T NIL)))
(|Depth-Constant| (|_Depth-Constant37897|
                   (((|_Depth-Constant37897| |instance-of|
                      |Depth-Constant|))
                    T NIL)))
(|Language| (|_Language34734|
             (((|_Language34734| |instance-of| |Language|)) T NIL)))
(|Thickness-Scale| (|_Thickness-Scale37517|
                    (((|_Thickness-Scale37517| |instance-of|
                       |Thickness-Scale|)
                      (|_Number37519| |instance-of| |Number|)
                      (|_Thickness-Scale37517| |number-of-elements|
                       |_Number37519|))
                     T NIL)))
(|Artifact| (|_Artifact34145|
             (((|_Time-Interval34150| |instance-of| |Time-Interval|)
               (|_Artifact34145| |instance-of| |Artifact|)
               (|_Create34147| |instance-of| |Create|)
               (|_Create34147| |actions| |_Create34147|)
               (|_Create34147| |primitive-actions| |_Create34147|)
               (|_Create34147| |time-during| |_Time-Interval34150|)
               (|_Artifact34145| |result-of| |_Create34147|))
              T NIL)))
(|Property-Relation| (|_Property-Relation37955|
                      (((|_Property-Relation37955| |instance-of|
                         |Property-Relation|))
                       T NIL)))
(|Latitude-Constant| (|_Latitude-Constant37875|
                      (((|_Latitude-Constant37875| |instance-of|
                         |Latitude-Constant|))
                       T NIL)))
(|Constant-Exclusion-Set| (|_Constant-Exclusion-Set34978|
                           (((|_Constant-Exclusion-Set34978|
                              |instance-of| |Constant-Exclusion-Set|))
                            T NIL)))
(|Gesticulate| (|_Gesticulate25697|
                (((|_Move25712| |instance-of| |Move|)
                  (|_Time-Interval25711| |instance-of| |Time-Interval|)
                  (|_Message25709| |instance-of| |Message|)
                  (|_Message25709| |instance-of| |Tangible-Entity|)
                  (|_Light25710| |instance-of| |Light|)
                  (|_Tangible-Entity25705| |instance-of|
                   |Tangible-Entity|)
                  (|_Gesticulate25697| |instance-of| |Gesticulate|)
                  (|_Tangible-Entity25706| |instance-of|
                   |Tangible-Entity|)
                  (|_Tangible-Entity25706| |abuts|
                   |_Tangible-Entity25705|)
                  (|_Gesticulate25697| |actions| |_Gesticulate25697|)
                  (|_Gesticulate25697| |preparatory-event|
                   |_Move25712|)
                  (|_Gesticulate25697| |primitive-actions|
                   |_Gesticulate25697|)
                  (|_Gesticulate25697| |time-during|
                   |_Time-Interval25711|)
                  (|_Gesticulate25697| |object| |_Message25709|)
                  (|_Gesticulate25697| |result| |_Light25710|)
                  (|_Gesticulate25697| |agent| |_Tangible-Entity25705|)
                  (|_Gesticulate25697| |instrument|
                   |_Tangible-Entity25706|))
                 T NIL)))
(|Acceleration-Magnitude-Value| (|_Acceleration-Magnitude-Value37730|
                                 (((|_Acceleration-Magnitude-Value37730|
                                    |instance-of|
                                    |Acceleration-Magnitude-Value|))
                                  T
                                  NIL)))
(|Arithmetic-Difference| (|_Arithmetic-Difference34972|
                          (((|_Arithmetic-Difference34972|
                             |instance-of| |Arithmetic-Difference|))
                           T NIL)))
(|Detach| (|_Detach25348|
           (((|_Move25354| |instance-of| |Move|)
             (|_Attach25353| |instance-of| |Attach|)
             (|_Time-Interval25352| |instance-of| |Time-Interval|)
             (|_Tangible-Entity25351| |instance-of| |Tangible-Entity|)
             (|_Detach25348| |instance-of| |Detach|)
             (|_Tangible-Entity25349| |instance-of| |Tangible-Entity|)
             (|_Detach25348| |actions| |_Detach25348|)
             (|_Detach25348| |preparatory-event| |_Move25354|)
             (|_Detach25348| |preparatory-event| |_Attach25353|)
             (|_Detach25348| |primitive-actions| |_Detach25348|)
             (|_Detach25348| |time-during| |_Time-Interval25352|)
             (|_Detach25348| |base| |_Tangible-Entity25351|)
             (|_Detach25348| |object| |_Tangible-Entity25349|))
            T NIL)))
(|Moment-of-Inertia-Value| (|_Moment-of-Inertia-Value37718|
                            (((|_Moment-of-Inertia-Value37718|
                               |instance-of|
                               |Moment-of-Inertia-Value|))
                             T NIL)))
(|Elevator| (|_Elevator34323|
             (((|_Time-Interval34364| |instance-of| |Time-Interval|)
               (|_Time-Interval34361| |instance-of| |Time-Interval|)
               (|_Time-Interval34358| |instance-of| |Time-Interval|)
               (|_Time-Interval34353| |instance-of| |Time-Interval|)
               (|_Time-Interval34350| |instance-of| |Time-Interval|)
               (|_Time-Interval34347| |instance-of| |Time-Interval|)
               (|_Scalar34334| |instance-of| |Scalar|)
               (|_Be-Stable34338| |instance-of| |Be-Stable|)
               (|_Be-Supported34337| |instance-of| |Be-Supported|)
               (|_Cover34336| |instance-of| |Cover|)
               (|_Create34335| |instance-of| |Create|)
               (|_Angle-Value34333| |instance-of| |Angle-Value|)
               (|_Be-Stable34332| |instance-of| |Be-Stable|)
               (|_Be-Supported34331| |instance-of| |Be-Supported|)
               (|_Container34330| |instance-of| |Container|)
               (|_Create34329| |instance-of| |Create|)
               (|_Ceiling34328| |instance-of| |Ceiling|)
               (|_Wall34327| |instance-of| |Wall|)
               (|_Floor34326| |instance-of| |Floor|)
               (|_Elevator34323| |instance-of| |Elevator|)
               (|_Building34325| |instance-of| |Building|)
               (|_Be-Stable34332| |actions| |_Be-Stable34332|)
               (|_Be-Stable34332| |primitive-actions|
                |_Be-Stable34332|)
               (|_Be-Stable34332| |time-during| |_Time-Interval34364|)
               (|_Be-Supported34331| |actions| |_Be-Supported34331|)
               (|_Be-Supported34331| |primitive-actions|
                |_Be-Supported34331|)
               (|_Be-Supported34331| |time-during|
                |_Time-Interval34361|)
               (|_Create34329| |actions| |_Create34329|)
               (|_Create34329| |primitive-actions| |_Create34329|)
               (|_Create34329| |time-during| |_Time-Interval34358|)
               (|_Be-Stable34338| |actions| |_Be-Stable34338|)
               (|_Be-Stable34338| |primitive-actions|
                |_Be-Stable34338|)
               (|_Be-Stable34338| |time-during| |_Time-Interval34353|)
               (|_Be-Supported34337| |actions| |_Be-Supported34337|)
               (|_Be-Supported34337| |primitive-actions|
                |_Be-Supported34337|)
               (|_Be-Supported34337| |time-during|
                |_Time-Interval34350|)
               (|_Create34335| |actions| |_Create34335|)
               (|_Create34335| |primitive-actions| |_Create34335|)
               (|_Create34335| |time-during| |_Time-Interval34347|)
               (|_Angle-Value34333| |scalar-value| |_Scalar34334|)
               (|_Ceiling34328| |is-above| |_Elevator34323|)
               (|_Ceiling34328| |object-of| |_Be-Stable34338|)
               (|_Ceiling34328| |object-of| |_Be-Supported34337|)
               (|_Ceiling34328| |plays| |_Cover34336|)
               (|_Ceiling34328| |result-of| |_Create34335|)
               (|_Ceiling34328| |orientation| |_Angle-Value34333|)
               (|_Elevator34323| |object-of| |_Be-Stable34332|)
               (|_Elevator34323| |object-of| |_Be-Supported34331|)
               (|_Elevator34323| |plays| |_Container34330|)
               (|_Elevator34323| |result-of| |_Create34329|)
               (|_Elevator34323| |has-part| |_Ceiling34328|)
               (|_Elevator34323| |has-part| |_Wall34327|)
               (|_Elevator34323| |has-part| |_Floor34326|)
               (|_Elevator34323| |is-part-of| |_Building34325|))
              T NIL)))
(|Intensity-Constant| (|_Intensity-Constant37877|
                       (((|_Intensity-Constant37877| |instance-of|
                          |Intensity-Constant|))
                        T NIL)))
(|Write| (|_Write25067|
          (((|_Move25078| |instance-of| |Move|)
            (|_Time-Interval25077| |instance-of| |Time-Interval|)
            (|_Information25075| |instance-of| |Information|)
            (|_Information25075| |instance-of| |Spatial-Entity|)
            (|_Document25076| |instance-of| |Document|)
            (|_Write25067| |instance-of| |Write|)
            (|_Tangible-Entity25073| |instance-of| |Tangible-Entity|)
            (|_Write25067| |actions| |_Write25067|)
            (|_Write25067| |preparatory-event| |_Move25078|)
            (|_Write25067| |primitive-actions| |_Write25067|)
            (|_Write25067| |time-during| |_Time-Interval25077|)
            (|_Write25067| |object| |_Information25075|)
            (|_Write25067| |result| |_Document25076|)
            (|_Write25067| |agent| |_Tangible-Entity25073|))
           T NIL)))
(|Be-Inaccessible| (|_Be-Inaccessible250|
                    (((|_Time-Interval254| |instance-of|
                       |Time-Interval|)
                      (|_Be-Inaccessible250| |instance-of|
                       |Be-Inaccessible|)
                      (|_Entity253| |instance-of| |Entity|)
                      (|_Be-Inaccessible250| |actions|
                       |_Be-Inaccessible250|)
                      (|_Be-Inaccessible250| |primitive-actions|
                       |_Be-Inaccessible250|)
                      (|_Be-Inaccessible250| |time-during|
                       |_Time-Interval254|)
                      (|_Be-Inaccessible250| |object| |_Entity253|))
                     T NIL)))
(|Break-Contact| (|_Break-Contact32751|
                  (((|_Move32757| |instance-of| |Move|)
                    (|_Detach32756| |instance-of| |Detach|)
                    (|_Time-Interval32755| |instance-of|
                     |Time-Interval|)
                    (|_Tangible-Entity32754| |instance-of|
                     |Tangible-Entity|)
                    (|_Break-Contact32751| |instance-of|
                     |Break-Contact|)
                    (|_Tangible-Entity32752| |instance-of|
                     |Tangible-Entity|)
                    (|_Break-Contact32751| |actions|
                     |_Break-Contact32751|)
                    (|_Break-Contact32751| |preparatory-event|
                     |_Move32757|)
                    (|_Break-Contact32751| |preparatory-event|
                     |_Detach32756|)
                    (|_Break-Contact32751| |primitive-actions|
                     |_Break-Contact32751|)
                    (|_Break-Contact32751| |time-during|
                     |_Time-Interval32755|)
                    (|_Break-Contact32751| |base|
                     |_Tangible-Entity32754|)
                    (|_Break-Contact32751| |object|
                     |_Tangible-Entity32752|))
                   T NIL)))
(|Contract| (|_Contract25431|
             (((|_Time-Interval25442| |instance-of| |Time-Interval|)
               (|_Tangible-Entity25441| |instance-of|
                |Tangible-Entity|)
               (|_Volume-Value25437| |instance-of| |Volume-Value|)
               (|_Contract25431| |instance-of| |Contract|)
               (|_Volume-Value25438| |instance-of| |Volume-Value|)
               (|_Volume-Value25438| |less-than| |_Volume-Value25437|)
               (|_Contract25431| |actions| |_Contract25431|)
               (|_Contract25431| |primitive-actions| |_Contract25431|)
               (|_Contract25431| |time-during| |_Time-Interval25442|)
               (|_Contract25431| |base| |_Tangible-Entity25441|)
               (|_Contract25431| |from-value| |_Volume-Value25437|)
               (|_Contract25431| |to-value| |_Volume-Value25438|))
              T NIL)))
(|Compute-Quantitative-Maximum| (|_Compute-Quantitative-Maximum34954|
                                 (((|_Compute-Quantitative-Maximum34954|
                                    |instance-of|
                                    |Compute-Quantitative-Maximum|))
                                  T
                                  NIL)))
(|Interaction| (|_Interaction776|
                (((|_Interaction776| |instance-of| |Interaction|)
                  (|_Time-Interval782| |instance-of| |Time-Interval|)
                  (|_Interaction776| |actions| |_Interaction776|)
                  (|_Interaction776| |primitive-actions|
                   |_Interaction776|)
                  (|_Interaction776| |time-during|
                   |_Time-Interval782|))
                 T NIL)))
(|SHAKEN-Attribute-Group| (|_SHAKEN-Attribute-Group37973|
                           (((|_SHAKEN-Attribute-Group37973|
                              |instance-of| |SHAKEN-Attribute-Group|))
                            T NIL)))
(|Coefficient-of-Friction-Scale| (|_Coefficient-of-Friction-Scale37662|
                                  (((|_Coefficient-of-Friction-Scale37662|
                                     |instance-of|
                                     |Coefficient-of-Friction-Scale|)
                                    (|_Number37664|
                                     |instance-of|
                                     |Number|)
                                    (|_Coefficient-of-Friction-Scale37662|
                                     |number-of-elements|
                                     |_Number37664|))
                                   T
                                   NIL)))
(|Record| (|_Record25563|
           (((|_Time-Interval25569| |instance-of| |Time-Interval|)
             (|_Record25563| |instance-of| |Record|)
             (|_Recording25567| |instance-of| |Recording|)
             (|_Record25563| |actions| |_Record25563|)
             (|_Record25563| |primitive-actions| |_Record25563|)
             (|_Record25563| |time-during| |_Time-Interval25569|)
             (|_Record25563| |result| |_Recording25567|))
            T NIL)))
(|Chemical-Formula| (|_Chemical-Formula34920|
                     (((|_Chemical-Formula34920| |instance-of|
                        |Chemical-Formula|))
                      T NIL)))
(|Be-Similar| (|_Be-Similar231|
               (((|_Be-Similar231| |instance-of| |Be-Similar|)
                 (|_Time-Interval233| |instance-of| |Time-Interval|)
                 (|_Be-Similar231| |actions| |_Be-Similar231|)
                 (|_Be-Similar231| |primitive-actions|
                  |_Be-Similar231|)
                 (|_Be-Similar231| |time-during| |_Time-Interval233|))
                T NIL)))
(|CLIB-Slot-Group| (|_CLIB-Slot-Group37967|
                    (((|_CLIB-Slot-Group37967| |instance-of|
                       |CLIB-Slot-Group|))
                     T NIL)))
(|Take-Control| (|_Take-Control1881|
                 (((|_Time-Interval1897| |instance-of| |Time-Interval|)
                   (|_Entity1893| |instance-of| |Entity|)
                   (|_Move1891| |instance-of| |Move|)
                   (|_Time-Interval1890| |instance-of| |Time-Interval|)
                   (|_Entity1888| |instance-of| |Spatial-Entity|)
                   (|_Be-Controlled1889| |instance-of| |Be-Controlled|)
                   (|_Take-Control1881| |instance-of| |Take-Control|)
                   (|_Entity1886| |instance-of| |Tangible-Entity|)
                   (|_Be-Controlled1889| |actions|
                    |_Be-Controlled1889|)
                   (|_Be-Controlled1889| |primitive-actions|
                    |_Be-Controlled1889|)
                   (|_Be-Controlled1889| |time-during|
                    |_Time-Interval1897|)
                   (|_Be-Controlled1889| |object| |_Entity1893|)
                   (|_Take-Control1881| |actions| |_Take-Control1881|)
                   (|_Take-Control1881| |preparatory-event|
                    |_Move1891|)
                   (|_Take-Control1881| |primitive-actions|
                    |_Take-Control1881|)
                   (|_Take-Control1881| |time-during|
                    |_Time-Interval1890|)
                   (|_Take-Control1881| |object| |_Entity1888|)
                   (|_Take-Control1881| |resulting-state|
                    |_Be-Controlled1889|)
                   (|_Take-Control1881| |agent| |_Entity1886|))
                  T NIL)))
(|Inanimate-Object| (|_Inanimate-Object34035|
                     (((|_Inanimate-Object34035| |instance-of|
                        |Inanimate-Object|))
                      T NIL)))
(|Piece-of-Paper| (|_Piece-of-Paper34106|
                   (((|_Piece-of-Paper34106| |instance-of|
                      |Piece-of-Paper|)
                     (|_Paper34109| |instance-of| |Paper|)
                     (|_Piece-of-Paper34106| |material| |_Paper34109|))
                    T NIL)))
(|Make-Accessible| (|_Make-Accessible24140|
                    (((|_Move24145| |instance-of| |Move|)
                      (|_Make-Inaccessible24144| |instance-of|
                       |Make-Inaccessible|)
                      (|_Time-Interval24143| |instance-of|
                       |Time-Interval|)
                      (|_Make-Accessible24140| |instance-of|
                       |Make-Accessible|)
                      (|_Entity24141| |instance-of| |Spatial-Entity|)
                      (|_Make-Accessible24140| |actions|
                       |_Make-Accessible24140|)
                      (|_Make-Accessible24140| |preparatory-event|
                       |_Move24145|)
                      (|_Make-Accessible24140| |preparatory-event|
                       |_Make-Inaccessible24144|)
                      (|_Make-Accessible24140| |primitive-actions|
                       |_Make-Accessible24140|)
                      (|_Make-Accessible24140| |time-during|
                       |_Time-Interval24143|)
                      (|_Make-Accessible24140| |object|
                       |_Entity24141|))
                     T NIL)))
(|Supply| (|_Supply1337|
           (((|_Move1376| |instance-of| |Move|)
             (|_Obtain1375| |instance-of| |Obtain|)
             (|_Time-Interval1374| |instance-of| |Time-Interval|)
             (|_Entity1371| |instance-of| |Tangible-Entity|)
             (|_Move1366| |instance-of| |Move|)
             (|_Obtain1365| |instance-of| |Obtain|)
             (|_Time-Interval1364| |instance-of| |Time-Interval|)
             (|_Resource1356| |instance-of| |Resource|)
             (|_Move1350| |instance-of| |Move|)
             (|_Obtain1349| |instance-of| |Obtain|)
             (|_Time-Interval1348| |instance-of| |Time-Interval|)
             (|_Tangible-Entity1346| |instance-of| |Tangible-Entity|)
             (|_Replenish1347| |instance-of| |Replenish|)
             (|_Supply1337| |instance-of| |Supply|)
             (|_Tangible-Entity1343| |instance-of| |Tangible-Entity|)
             (|_Obtain1365| |actions| |_Obtain1365|)
             (|_Obtain1365| |preparatory-event| |_Move1376|)
             (|_Obtain1365| |preparatory-event| |_Obtain1375|)
             (|_Obtain1365| |primitive-actions| |_Obtain1365|)
             (|_Obtain1365| |time-during| |_Time-Interval1374|)
             (|_Obtain1365| |object| |_Tangible-Entity1346|)
             (|_Obtain1365| |recipient| |_Entity1371|)
             (|_Obtain1365| |agent| |_Entity1371|)
             (|_Obtain1349| |actions| |_Obtain1349|)
             (|_Obtain1349| |preparatory-event| |_Move1366|)
             (|_Obtain1349| |preparatory-event| |_Obtain1365|)
             (|_Obtain1349| |primitive-actions| |_Obtain1349|)
             (|_Obtain1349| |time-during| |_Time-Interval1364|)
             (|_Obtain1349| |object| |_Tangible-Entity1346|)
             (|_Obtain1349| |recipient| |_Tangible-Entity1343|)
             (|_Obtain1349| |agent| |_Tangible-Entity1343|)
             (|_Tangible-Entity1346| |object-of| |_Replenish1347|)
             (|_Tangible-Entity1346| |plays| |_Resource1356|)
             (|_Supply1337| |actions| |_Supply1337|)
             (|_Supply1337| |preparatory-event| |_Move1350|)
             (|_Supply1337| |preparatory-event| |_Obtain1349|)
             (|_Supply1337| |primitive-actions| |_Supply1337|)
             (|_Supply1337| |time-during| |_Time-Interval1348|)
             (|_Supply1337| |object| |_Tangible-Entity1346|)
             (|_Supply1337| |enables| |_Replenish1347|)
             (|_Supply1337| |agent| |_Tangible-Entity1343|)
             (|_Supply1337| |donor| |_Tangible-Entity1343|))
            T NIL)))
(|Position-Constant| (|_Position-Constant37929|
                      (((|_Position-Constant37929| |instance-of|
                         |Position-Constant|))
                       T NIL)))
(|Target| (|_Target32942|
           (((|_Target32942| |instance-of| |Target|)
             (|_Spatial-Entity32943| |instance-of| |Spatial-Entity|)
             (|_Target32942| |played-by| |_Spatial-Entity32943|))
            T NIL)))
(|Cabinet| (|_Cabinet34194|
            (((|_Time-Interval34199| |instance-of| |Time-Interval|)
              (|_Cabinet34194| |instance-of| |Cabinet|)
              (|_Create34196| |instance-of| |Create|)
              (|_Create34196| |actions| |_Create34196|)
              (|_Create34196| |primitive-actions| |_Create34196|)
              (|_Create34196| |time-during| |_Time-Interval34199|)
              (|_Cabinet34194| |result-of| |_Create34196|))
             T NIL)))
(|Plastic| (|_Plastic33763|
            (((|_Categorical33768| |instance-of| |Categorical|)
              (|_Plastic33763| |instance-of| |Plastic|)
              (|_State-Value33767| |instance-of| |State-Value|)
              (|_State-Value33767| |categorical-value|
               |_Categorical33768|)
              (|_Plastic33763| |physical-state| |_State-Value33767|))
             T NIL)))
(|Wagon| (|_Wagon34637|
          (((|_Wagon34637| |instance-of| |Wagon|)) T NIL)))
(|UoM-Mass| (|_UoM-Mass37452|
             (((|_UoM-Mass37452| |instance-of| |UoM-Mass|)) T NIL)))
(|Be-Confined| (|_Be-Confined297|
                (((|_Time-Interval303| |instance-of| |Time-Interval|)
                  (|_Tangible-Entity301| |instance-of|
                   |Tangible-Entity|)
                  (|_Be-Confined297| |instance-of| |Be-Confined|)
                  (|_Tangible-Entity302| |instance-of|
                   |Tangible-Entity|)
                  (|_Tangible-Entity302| |is-inside|
                   |_Tangible-Entity301|)
                  (|_Be-Confined297| |actions| |_Be-Confined297|)
                  (|_Be-Confined297| |primitive-actions|
                   |_Be-Confined297|)
                  (|_Be-Confined297| |time-during| |_Time-Interval303|)
                  (|_Be-Confined297| |base| |_Tangible-Entity301|)
                  (|_Be-Confined297| |object| |_Tangible-Entity302|))
                 T NIL)))
(|Be-Concealed| (|_Be-Concealed306|
                 (((|_Time-Interval310| |instance-of| |Time-Interval|)
                   (|_Be-Concealed306| |instance-of| |Be-Concealed|)
                   (|_Tangible-Entity309| |instance-of|
                    |Tangible-Entity|)
                   (|_Be-Concealed306| |actions| |_Be-Concealed306|)
                   (|_Be-Concealed306| |primitive-actions|
                    |_Be-Concealed306|)
                   (|_Be-Concealed306| |time-during|
                    |_Time-Interval310|)
                   (|_Be-Concealed306| |object| |_Tangible-Entity309|))
                  T NIL)))
(|Width-Constant| (|_Width-Constant37801|
                   (((|_Width-Constant37801| |instance-of|
                      |Width-Constant|))
                    T NIL)))
(|Trait-Value| (|_Trait-Value37688|
                (((|_Trait-Value37688| |instance-of| |Trait-Value|)) T
                 NIL)))
(|Be-Different| (|_Be-Different330|
                 (((|_Be-Different330| |instance-of| |Be-Different|)
                   (|_Time-Interval332| |instance-of| |Time-Interval|)
                   (|_Be-Different330| |actions| |_Be-Different330|)
                   (|_Be-Different330| |primitive-actions|
                    |_Be-Different330|)
                   (|_Be-Different330| |time-during|
                    |_Time-Interval332|))
                  T NIL)))
(|Expand| (|_Expand24698|
           (((|_Time-Interval24709| |instance-of| |Time-Interval|)
             (|_Tangible-Entity24708| |instance-of| |Tangible-Entity|)
             (|_Volume-Value24704| |instance-of| |Volume-Value|)
             (|_Expand24698| |instance-of| |Expand|)
             (|_Volume-Value24705| |instance-of| |Volume-Value|)
             (|_Volume-Value24705| |greater-than| |_Volume-Value24704|)
             (|_Expand24698| |actions| |_Expand24698|)
             (|_Expand24698| |primitive-actions| |_Expand24698|)
             (|_Expand24698| |time-during| |_Time-Interval24709|)
             (|_Expand24698| |base| |_Tangible-Entity24708|)
             (|_Expand24698| |from-value| |_Volume-Value24704|)
             (|_Expand24698| |to-value| |_Volume-Value24705|))
            T NIL)))
(|Volume-Scale| (|_Volume-Scale37507|
                 (((|_Volume-Scale37507| |instance-of| |Volume-Scale|)
                   (|_Number37509| |instance-of| |Number|)
                   (|_Volume-Scale37507| |number-of-elements|
                    |_Number37509|))
                  T NIL)))
(|Waste| (|_Waste32906|
          (((|_Consume32909| |instance-of| |Consume|)
            (|_Waste32906| |instance-of| |Waste|)
            (|_Tangible-Entity32908| |instance-of| |Tangible-Entity|)
            (|_Waste32906| |in-event| |_Consume32909|)
            (|_Waste32906| |played-by| |_Tangible-Entity32908|))
           T NIL)))
(|Outlet| (|_Outlet33072|
           (((|_Move-Out-Of33075| |instance-of| |Move-Out-Of|)
             (|_Outlet33072| |instance-of| |Outlet|)
             (|_Spatial-Entity33074| |instance-of| |Spatial-Entity|)
             (|_Outlet33072| |in-event| |_Move-Out-Of33075|)
             (|_Outlet33072| |played-by| |_Spatial-Entity33074|))
            T NIL)))
(|Hollow-Ball| (|_Hollow-Ball34617|
                (((|_Hollow-Ball34617| |instance-of| |Hollow-Ball|)) T
                 NIL)))
(|UoM-Brightness| (|_UoM-Brightness37472|
                   (((|_UoM-Brightness37472| |instance-of|
                      |UoM-Brightness|))
                    T NIL)))
(|Bullet| (|_Bullet34607|
           (((|_Bullet34607| |instance-of| |Bullet|)) T NIL)))
(|Density-Value| (|_Density-Value37746|
                  (((|_Density-Value37746| |instance-of|
                     |Density-Value|))
                   T NIL)))
(|Class-Definition-Viewpoint| (|_Class-Definition-Viewpoint34936|
                               (((|_Class-Definition-Viewpoint34936|
                                  |instance-of|
                                  |Class-Definition-Viewpoint|))
                                T
                                NIL)))
(|ParkingLot| (|_ParkingLot34479|
               (((|_Time-Interval34492| |instance-of| |Time-Interval|)
                 (|_Time-Interval34489| |instance-of| |Time-Interval|)
                 (|_Time-Interval34486| |instance-of| |Time-Interval|)
                 (|_Be-Stable34483| |instance-of| |Be-Stable|)
                 (|_Be-Supported34482| |instance-of| |Be-Supported|)
                 (|_ParkingLot34479| |instance-of| |ParkingLot|)
                 (|_Create34481| |instance-of| |Create|)
                 (|_Be-Stable34483| |actions| |_Be-Stable34483|)
                 (|_Be-Stable34483| |primitive-actions|
                  |_Be-Stable34483|)
                 (|_Be-Stable34483| |time-during|
                  |_Time-Interval34492|)
                 (|_Be-Supported34482| |actions| |_Be-Supported34482|)
                 (|_Be-Supported34482| |primitive-actions|
                  |_Be-Supported34482|)
                 (|_Be-Supported34482| |time-during|
                  |_Time-Interval34489|)
                 (|_Create34481| |actions| |_Create34481|)
                 (|_Create34481| |primitive-actions| |_Create34481|)
                 (|_Create34481| |time-during| |_Time-Interval34486|)
                 (|_ParkingLot34479| |object-of| |_Be-Stable34483|)
                 (|_ParkingLot34479| |object-of| |_Be-Supported34482|)
                 (|_ParkingLot34479| |result-of| |_Create34481|))
                T NIL)))
(|UoM-Luminous-Flux| (|_UoM-Luminous-Flux37456|
                      (((|_UoM-Luminous-Flux37456| |instance-of|
                         |UoM-Luminous-Flux|))
                       T NIL)))
(|Sound| (|_Sound34641|
          (((|_Sound34641| |instance-of| |Sound|)
            (|_Move34642| |instance-of| |Move|)
            (|_Sound34641| |object-of| |_Move34642|))
           T NIL)))
(|Be-Known| (|_Be-Known241|
             (((|_Time-Interval246| |instance-of| |Time-Interval|)
               (|_Tangible-Entity245| |instance-of| |Living-Entity|)
               (|_Be-Known241| |instance-of| |Be-Known|)
               (|_Entity244| |instance-of| |Entity|)
               (|_Be-Known241| |actions| |_Be-Known241|)
               (|_Be-Known241| |primitive-actions| |_Be-Known241|)
               (|_Be-Known241| |time-during| |_Time-Interval246|)
               (|_Be-Known241| |experiencer| |_Tangible-Entity245|)
               (|_Be-Known241| |object| |_Entity244|))
              T NIL)))
(|Deterioration| (|_Deterioration911|
                  (((|_Deterioration911| |instance-of| |Deterioration|)
                    (|_Time-Interval917| |instance-of| |Time-Interval|)
                    (|_Deterioration911| |actions| |_Deterioration911|)
                    (|_Deterioration911| |primitive-actions|
                     |_Deterioration911|)
                    (|_Deterioration911| |time-during|
                     |_Time-Interval917|))
                   T NIL)))
(|Obtain| (|_Obtain1043|
           (((|_Move1049| |instance-of| |Move|)
             (|_Obtain1048| |instance-of| |Obtain|)
             (|_Time-Interval1047| |instance-of| |Time-Interval|)
             (|_Entity1046| |instance-of| |Spatial-Entity|)
             (|_Obtain1043| |instance-of| |Obtain|)
             (|_Entity1044| |instance-of| |Tangible-Entity|)
             (|_Obtain1043| |actions| |_Obtain1043|)
             (|_Obtain1043| |preparatory-event| |_Move1049|)
             (|_Obtain1043| |preparatory-event| |_Obtain1048|)
             (|_Obtain1043| |primitive-actions| |_Obtain1043|)
             (|_Obtain1043| |time-during| |_Time-Interval1047|)
             (|_Obtain1043| |object| |_Entity1046|)
             (|_Obtain1043| |recipient| |_Entity1044|)
             (|_Obtain1043| |agent| |_Entity1044|))
            T NIL)))
(|Resource| (|_Resource32961|
             (((|_Resource32961| |instance-of| |Resource|)) T NIL)))
(|Tissue| (|_Tissue33670|
           (((|_Categorical33675| |instance-of| |Categorical|)
             (|_Tissue33670| |instance-of| |Tissue|)
             (|_State-Value33674| |instance-of| |State-Value|)
             (|_State-Value33674| |categorical-value|
              |_Categorical33675|)
             (|_Tissue33670| |physical-state| |_State-Value33674|))
            T NIL)))
(|Rectangle| (|_Rectangle34716|
              (((|_Rectangle34716| |instance-of| |Rectangle|)) T NIL)))
(|Portal-Covering| (|_Portal-Covering33140|
                    (((|_Block33142| |instance-of| |Block|)
                      (|_Portal-Covering33140| |instance-of|
                       |Portal-Covering|)
                      (|_Tangible-Entity33141| |instance-of|
                       |Tangible-Entity|)
                      (|_Portal-Covering33140| |in-event|
                       |_Block33142|)
                      (|_Portal-Covering33140| |played-by|
                       |_Tangible-Entity33141|))
                     T NIL)))
(|Fall| (|_Fall23102|
         (((|_Move23135| |instance-of| |Move|)
           (|_Move23134| |instance-of| |Move|)
           (|_Support23133| |instance-of| |Support|)
           (|_Time-Interval23132| |instance-of| |Time-Interval|)
           (|_Spatial-Entity23104| |instance-of| |Spatial-Entity|)
           (|_Spatial-Entity23103| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity23117| |instance-of| |Tangible-Entity|)
           (|_Acceleration-Magnitude-Value23107| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value23106| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value23112| |instance-of| |Length-Value|)
           (|_Duration-Value23105| |instance-of| |Duration-Value|)
           (|_Speed-Value23109| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value23113| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value23131| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23128| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23130| |instance-of| |Speed-Value|)
           (|_Speed-Value23129| |instance-of| |Speed-Value|)
           (|_Speed-Value23127| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23124| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23126| |instance-of| |Speed-Value|)
           (|_Speed-Value23125| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23108| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value23123| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23122| |instance-of| |Length-Value|)
           (|_Speed-Value23121| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value23120| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23119| |instance-of| |Length-Value|)
           (|_Fall23102| |instance-of| |Fall|)
           (|_Speed-Value23118| |instance-of| |Speed-Value|)
           (|_Fall23102| |actions| |_Fall23102|)
           (|_Fall23102| |preparatory-event| |_Move23135|)
           (|_Fall23102| |preparatory-event| |_Move23134|)
           (|_Fall23102| |preparatory-event| |_Support23133|)
           (|_Fall23102| |primitive-actions| |_Fall23102|)
           (|_Fall23102| |time-during| |_Time-Interval23132|)
           (|_Fall23102| |destination| |_Spatial-Entity23104|)
           (|_Fall23102| |origin| |_Spatial-Entity23103|)
           (|_Fall23102| |object| |_Tangible-Entity23117|)
           (|_Fall23102| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value23107|)
           (|_Fall23102| |acceleration|
            |_Acceleration-Vector-Value23106|)
           (|_Fall23102| |distance| |_Length-Value23112|)
           (|_Fall23102| |duration| |_Duration-Value23105|)
           (|_Fall23102| |speed| |_Speed-Value23109|)
           (|_Fall23102| |displacement|
            |_Displacement-Vector-Value23113|)
           (|_Fall23102| |final-speed| |_Speed-Value23131|)
           (|_Fall23102| |final-velocity|
            |_Velocity-Vector-Value23128|)
           (|_Fall23102| |final-x-speed| |_Speed-Value23130|)
           (|_Fall23102| |final-y-speed| |_Speed-Value23129|)
           (|_Fall23102| |initial-speed| |_Speed-Value23127|)
           (|_Fall23102| |initial-velocity|
            |_Velocity-Vector-Value23124|)
           (|_Fall23102| |initial-x-speed| |_Speed-Value23126|)
           (|_Fall23102| |initial-y-speed| |_Speed-Value23125|)
           (|_Fall23102| |velocity| |_Velocity-Vector-Value23108|)
           (|_Fall23102| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23123|)
           (|_Fall23102| |x-distance| |_Length-Value23122|)
           (|_Fall23102| |x-speed| |_Speed-Value23121|)
           (|_Fall23102| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23120|)
           (|_Fall23102| |y-distance| |_Length-Value23119|)
           (|_Fall23102| |y-speed| |_Speed-Value23118|))
          T NIL)))
(|Distributing| (|_Distributing866|
                 (((|_Distributing866| |instance-of| |Distributing|)
                   (|_Time-Interval872| |instance-of| |Time-Interval|)
                   (|_Distributing866| |actions| |_Distributing866|)
                   (|_Distributing866| |primitive-actions|
                    |_Distributing866|)
                   (|_Distributing866| |time-during|
                    |_Time-Interval872|))
                  T NIL)))
(|Meal| (|_Meal525|
         (((|_Time-Interval532| |instance-of| |Time-Interval|)
           (|_Meal525| |instance-of| |Meal|)
           (|_Eat531| |instance-of| |Eat|)
           (|_Meal525| |actions| |_Meal525|)
           (|_Meal525| |actions| |_Eat531|)
           (|_Meal525| |all-subevents| |_Eat531|)
           (|_Meal525| |primitive-actions| |_Eat531|)
           (|_Meal525| |time-during| |_Time-Interval532|)
           (|_Meal525| |subevent| |_Eat531|))
          T NIL)))
(|Cyclist| (|_Cyclist33552|
            (((|_Locomotion33555| |instance-of| |Locomotion|)
              (|_Cyclist33552| |instance-of| |Cyclist|)
              (|_Person33554| |instance-of| |Person|)
              (|_Cyclist33552| |in-event| |_Locomotion33555|)
              (|_Cyclist33552| |played-by| |_Person33554|))
             T NIL)))
(|Wall| (|_Wall34227|
         (((|_Time-Interval34250| |instance-of| |Time-Interval|)
           (|_Time-Interval34247| |instance-of| |Time-Interval|)
           (|_Time-Interval34244| |instance-of| |Time-Interval|)
           (|_Scalar34230| |instance-of| |Scalar|)
           (|_Be-Stable34234| |instance-of| |Be-Stable|)
           (|_Be-Supported34233| |instance-of| |Be-Supported|)
           (|_Barrier34232| |instance-of| |Barrier|)
           (|_Create34231| |instance-of| |Create|)
           (|_Wall34227| |instance-of| |Wall|)
           (|_Angle-Value34229| |instance-of| |Angle-Value|)
           (|_Be-Stable34234| |actions| |_Be-Stable34234|)
           (|_Be-Stable34234| |primitive-actions| |_Be-Stable34234|)
           (|_Be-Stable34234| |time-during| |_Time-Interval34250|)
           (|_Be-Supported34233| |actions| |_Be-Supported34233|)
           (|_Be-Supported34233| |primitive-actions|
            |_Be-Supported34233|)
           (|_Be-Supported34233| |time-during| |_Time-Interval34247|)
           (|_Create34231| |actions| |_Create34231|)
           (|_Create34231| |primitive-actions| |_Create34231|)
           (|_Create34231| |time-during| |_Time-Interval34244|)
           (|_Angle-Value34229| |scalar-value| |_Scalar34230|)
           (|_Wall34227| |object-of| |_Be-Stable34234|)
           (|_Wall34227| |object-of| |_Be-Supported34233|)
           (|_Wall34227| |plays| |_Barrier34232|)
           (|_Wall34227| |result-of| |_Create34231|)
           (|_Wall34227| |orientation| |_Angle-Value34229|))
          T NIL)))
(|Cover| (|_Cover33146|
          (((|_Time-Interval33166| |instance-of| |Time-Interval|)
            (|_Spatial-Entity33162| |instance-of| |Spatial-Entity|)
            (|_Move33160| |instance-of| |Move|)
            (|_Unblock33158| |instance-of| |Unblock|)
            (|_Time-Interval33157| |instance-of| |Time-Interval|)
            (|_Spatial-Entity33154| |instance-of| |Spatial-Entity|)
            (|_Be-Blocked33156| |instance-of| |Be-Blocked|)
            (|_Spatial-Entity33150| |instance-of| |Spatial-Entity|)
            (|_Spatial-Entity33152| |instance-of| |Spatial-Entity|)
            (|_Spatial-Entity33151| |instance-of| |Spatial-Entity|)
            (|_Block33149| |instance-of| |Block|)
            (|_Cover33146| |instance-of| |Cover|)
            (|_Tangible-Entity33148| |instance-of| |Tangible-Entity|)
            (|_Be-Blocked33156| |object| |_Spatial-Entity33154|)
            (|_Be-Blocked33156| |actions| |_Be-Blocked33156|)
            (|_Be-Blocked33156| |primitive-actions| |_Be-Blocked33156|)
            (|_Be-Blocked33156| |time-during| |_Time-Interval33166|)
            (|_Be-Blocked33156| |object| |_Spatial-Entity33162|)
            (|_Block33149| |actions| |_Block33149|)
            (|_Block33149| |preparatory-event| |_Move33160|)
            (|_Block33149| |preparatory-event| |_Unblock33158|)
            (|_Block33149| |primitive-actions| |_Block33149|)
            (|_Block33149| |time-during| |_Time-Interval33157|)
            (|_Block33149| |object| |_Spatial-Entity33154|)
            (|_Block33149| |resulting-state| |_Be-Blocked33156|)
            (|_Tangible-Entity33148| |is-above| |_Spatial-Entity33150|)
            (|_Tangible-Entity33148| |is-between|
             |_Spatial-Entity33152|)
            (|_Tangible-Entity33148| |is-between|
             |_Spatial-Entity33151|)
            (|_Tangible-Entity33148| |instrument-of| |_Block33149|)
            (|_Cover33146| |in-event| |_Block33149|)
            (|_Cover33146| |played-by| |_Tangible-Entity33148|))
           T NIL)))
(|Workgroup| (|_Workgroup34862|
              (((|_Workgroup34862| |instance-of| |Workgroup|)
                (|_Number34864| |instance-of| |Number|)
                (|_Workgroup34862| |number-of-elements|
                 |_Number34864|))
               T NIL)))
(|Interpret| (|_Interpret24583|
              (((|_Time-Interval24600| |instance-of| |Time-Interval|)
                (|_Tangible-Entity24599| |instance-of| |Living-Entity|)
                (|_Entity24595| |instance-of| |Entity|)
                (|_Move24593| |instance-of| |Move|)
                (|_Time-Interval24592| |instance-of| |Time-Interval|)
                (|_Message24590| |instance-of| |Message|)
                (|_Message24590| |instance-of| |Spatial-Entity|)
                (|_Be-Known24591| |instance-of| |Be-Known|)
                (|_Interpret24583| |instance-of| |Interpret|)
                (|_Tangible-Entity24588| |instance-of|
                 |Tangible-Entity|)
                (|_Be-Known24591| |actions| |_Be-Known24591|)
                (|_Be-Known24591| |primitive-actions| |_Be-Known24591|)
                (|_Be-Known24591| |time-during| |_Time-Interval24600|)
                (|_Be-Known24591| |experiencer|
                 |_Tangible-Entity24599|)
                (|_Be-Known24591| |object| |_Entity24595|)
                (|_Interpret24583| |actions| |_Interpret24583|)
                (|_Interpret24583| |preparatory-event| |_Move24593|)
                (|_Interpret24583| |primitive-actions|
                 |_Interpret24583|)
                (|_Interpret24583| |time-during| |_Time-Interval24592|)
                (|_Interpret24583| |object| |_Message24590|)
                (|_Interpret24583| |resulting-state| |_Be-Known24591|)
                (|_Interpret24583| |agent| |_Tangible-Entity24588|))
               T NIL)))
(|Shape-Constant| (|_Shape-Constant37833|
                   (((|_Shape-Constant37833| |instance-of|
                      |Shape-Constant|))
                    T NIL)))
(|Line| (|_Line34693| (((|_Line34693| |instance-of| |Line|)) T NIL)))
(|Move-Into| (|_Move-Into20063|
              (((|_Portal20088| |instance-of| |Portal|)
                (|_Container20146| |instance-of| |Container|)
                (|_Time-Interval20143| |instance-of| |Time-Interval|)
                (|_Spatial-Entity20138| |instance-of| |Spatial-Entity|)
                (|_Spatial-Entity20137| |instance-of| |Spatial-Entity|)
                (|_Tangible-Entity20142| |instance-of|
                 |Tangible-Entity|)
                (|_Tangible-Entity20135| |instance-of|
                 |Tangible-Entity|)
                (|_Move20119| |instance-of| |Move|)
                (|_Move20118| |instance-of| |Move|)
                (|_Admit20117| |instance-of| |Admit|)
                (|_Time-Interval20116| |instance-of| |Time-Interval|)
                (|_Spatial-Entity20084| |instance-of| |Spatial-Entity|)
                (|_Spatial-Entity20083| |instance-of| |Spatial-Entity|)
                (|_Spatial-Entity20085| |instance-of| |Spatial-Entity|)
                (|_Tangible-Entity20086| |instance-of|
                 |Tangible-Entity|)
                (|_Tangible-Entity20100| |instance-of|
                 |Tangible-Entity|)
                (|_Be-Contained20115| |instance-of| |Be-Contained|)
                (|_Acceleration-Magnitude-Value20091| |instance-of|
                 |Acceleration-Magnitude-Value|)
                (|_Acceleration-Vector-Value20090| |instance-of|
                 |Acceleration-Vector-Value|)
                (|_Length-Value20097| |instance-of| |Length-Value|)
                (|_Duration-Value20089| |instance-of| |Duration-Value|)
                (|_Speed-Value20093| |instance-of| |Speed-Value|)
                (|_Displacement-Vector-Value20096| |instance-of|
                 |Displacement-Vector-Value|)
                (|_Speed-Value20114| |instance-of| |Speed-Value|)
                (|_Velocity-Vector-Value20111| |instance-of|
                 |Velocity-Vector-Value|)
                (|_Speed-Value20113| |instance-of| |Speed-Value|)
                (|_Speed-Value20112| |instance-of| |Speed-Value|)
                (|_Speed-Value20110| |instance-of| |Speed-Value|)
                (|_Velocity-Vector-Value20107| |instance-of|
                 |Velocity-Vector-Value|)
                (|_Speed-Value20109| |instance-of| |Speed-Value|)
                (|_Speed-Value20108| |instance-of| |Speed-Value|)
                (|_Velocity-Vector-Value20092| |instance-of|
                 |Velocity-Vector-Value|)
                (|_Acceleration-Magnitude-Value20106| |instance-of|
                 |Acceleration-Magnitude-Value|)
                (|_Length-Value20105| |instance-of| |Length-Value|)
                (|_Speed-Value20104| |instance-of| |Speed-Value|)
                (|_Acceleration-Magnitude-Value20103| |instance-of|
                 |Acceleration-Magnitude-Value|)
                (|_Length-Value20102| |instance-of| |Length-Value|)
                (|_Move-Into20063| |instance-of| |Move-Into|)
                (|_Speed-Value20101| |instance-of| |Speed-Value|)
                (|_Spatial-Entity20083| |is-outside|
                 |_Tangible-Entity20086|)
                (|_Tangible-Entity20086| |base-of|
                 |_Be-Contained20115|)
                (|_Spatial-Entity20085| |plays| |_Portal20088|)
                (|_Tangible-Entity20086| |encloses|
                 |_Spatial-Entity20084|)
                (|_Tangible-Entity20086| |plays| |_Container20146|)
                (|_Tangible-Entity20086| |has-region|
                 |_Spatial-Entity20085|)
                (|_Be-Contained20115| |actions| |_Be-Contained20115|)
                (|_Be-Contained20115| |primitive-actions|
                 |_Be-Contained20115|)
                (|_Be-Contained20115| |time-during|
                 |_Time-Interval20143|)
                (|_Be-Contained20115| |destination|
                 |_Spatial-Entity20138|)
                (|_Be-Contained20115| |origin| |_Spatial-Entity20137|)
                (|_Be-Contained20115| |base| |_Tangible-Entity20142|)
                (|_Be-Contained20115| |object| |_Tangible-Entity20135|)
                (|_Move-Into20063| |actions| |_Move-Into20063|)
                (|_Move-Into20063| |preparatory-event| |_Move20119|)
                (|_Move-Into20063| |preparatory-event| |_Move20118|)
                (|_Move-Into20063| |preparatory-event| |_Admit20117|)
                (|_Move-Into20063| |primitive-actions|
                 |_Move-Into20063|)
                (|_Move-Into20063| |time-during| |_Time-Interval20116|)
                (|_Move-Into20063| |destination|
                 |_Spatial-Entity20084|)
                (|_Move-Into20063| |origin| |_Spatial-Entity20083|)
                (|_Move-Into20063| |path| |_Spatial-Entity20085|)
                (|_Move-Into20063| |base| |_Tangible-Entity20086|)
                (|_Move-Into20063| |object| |_Tangible-Entity20100|)
                (|_Move-Into20063| |resulting-state|
                 |_Be-Contained20115|)
                (|_Move-Into20063| |acceleration-magnitude|
                 |_Acceleration-Magnitude-Value20091|)
                (|_Move-Into20063| |acceleration|
                 |_Acceleration-Vector-Value20090|)
                (|_Move-Into20063| |distance| |_Length-Value20097|)
                (|_Move-Into20063| |duration| |_Duration-Value20089|)
                (|_Move-Into20063| |speed| |_Speed-Value20093|)
                (|_Move-Into20063| |displacement|
                 |_Displacement-Vector-Value20096|)
                (|_Move-Into20063| |final-speed| |_Speed-Value20114|)
                (|_Move-Into20063| |final-velocity|
                 |_Velocity-Vector-Value20111|)
                (|_Move-Into20063| |final-x-speed| |_Speed-Value20113|)
                (|_Move-Into20063| |final-y-speed| |_Speed-Value20112|)
                (|_Move-Into20063| |initial-speed| |_Speed-Value20110|)
                (|_Move-Into20063| |initial-velocity|
                 |_Velocity-Vector-Value20107|)
                (|_Move-Into20063| |initial-x-speed|
                 |_Speed-Value20109|)
                (|_Move-Into20063| |initial-y-speed|
                 |_Speed-Value20108|)
                (|_Move-Into20063| |velocity|
                 |_Velocity-Vector-Value20092|)
                (|_Move-Into20063| |x-acceleration-magnitude|
                 |_Acceleration-Magnitude-Value20106|)
                (|_Move-Into20063| |x-distance| |_Length-Value20105|)
                (|_Move-Into20063| |x-speed| |_Speed-Value20104|)
                (|_Move-Into20063| |y-acceleration-magnitude|
                 |_Acceleration-Magnitude-Value20103|)
                (|_Move-Into20063| |y-distance| |_Length-Value20102|)
                (|_Move-Into20063| |y-speed| |_Speed-Value20101|))
               T NIL)))
(|Suggestion| (|_Suggestion34799|
               (((|_Suggestion34799| |instance-of| |Suggestion|)
                 (|_Thing34800| |instance-of| |Thing|)
                 (|_Suggestion34799| |information-content|
                  |_Thing34800|))
                T NIL)))
(|Wetness-Value| (|_Wetness-Value37676|
                  (((|_Wetness-Value37676| |instance-of|
                     |Wetness-Value|))
                   T NIL)))
(|Allocate-Resource| (|_Allocate-Resource32833|
                      (((|_Move32838| |instance-of| |Move|)
                        (|_Release-Resource32837| |instance-of|
                         |Release-Resource|)
                        (|_Time-Interval32836| |instance-of|
                         |Time-Interval|)
                        (|_Allocate-Resource32833| |instance-of|
                         |Allocate-Resource|)
                        (|_Resource32834| |instance-of| |Resource|)
                        (|_Resource32834| |instance-of|
                         |Spatial-Entity|)
                        (|_Allocate-Resource32833| |actions|
                         |_Allocate-Resource32833|)
                        (|_Allocate-Resource32833| |preparatory-event|
                         |_Move32838|)
                        (|_Allocate-Resource32833| |preparatory-event|
                         |_Release-Resource32837|)
                        (|_Allocate-Resource32833| |primitive-actions|
                         |_Allocate-Resource32833|)
                        (|_Allocate-Resource32833| |time-during|
                         |_Time-Interval32836|)
                        (|_Allocate-Resource32833| |object|
                         |_Resource32834|))
                       T NIL)))
(|Sex-Constant| (|_Sex-Constant37835|
                 (((|_Sex-Constant37835| |instance-of| |Sex-Constant|))
                  T NIL)))
(|Scalar| (|_Scalar37672|
           (((|_Scalar37672| |instance-of| |Scalar|)) T NIL)))
(|Piece-of-Plastic| (|_Piece-of-Plastic34093|
                     (((|_Piece-of-Plastic34093| |instance-of|
                        |Piece-of-Plastic|)
                       (|_Plastic34096| |instance-of| |Plastic|)
                       (|_Piece-of-Plastic34093| |material|
                        |_Plastic34096|))
                      T NIL)))
(|Conceal| (|_Conceal23911|
            (((|_Time-Interval23927| |instance-of| |Time-Interval|)
              (|_Tangible-Entity23923| |instance-of| |Tangible-Entity|)
              (|_Move23921| |instance-of| |Move|)
              (|_Expose23919| |instance-of| |Expose|)
              (|_Time-Interval23918| |instance-of| |Time-Interval|)
              (|_Tangible-Entity23915| |instance-of| |Tangible-Entity|)
              (|_Conceal23911| |instance-of| |Conceal|)
              (|_Be-Concealed23917| |instance-of| |Be-Concealed|)
              (|_Be-Concealed23917| |actions| |_Be-Concealed23917|)
              (|_Be-Concealed23917| |primitive-actions|
               |_Be-Concealed23917|)
              (|_Be-Concealed23917| |time-during|
               |_Time-Interval23927|)
              (|_Be-Concealed23917| |object| |_Tangible-Entity23923|)
              (|_Conceal23911| |actions| |_Conceal23911|)
              (|_Conceal23911| |preparatory-event| |_Move23921|)
              (|_Conceal23911| |preparatory-event| |_Expose23919|)
              (|_Conceal23911| |primitive-actions| |_Conceal23911|)
              (|_Conceal23911| |time-during| |_Time-Interval23918|)
              (|_Conceal23911| |object| |_Tangible-Entity23915|)
              (|_Conceal23911| |resulting-state| |_Be-Concealed23917|))
             T NIL)))
(|Bridge| (|_Bridge34572|
           (((|_Barrier34577| |instance-of| |Barrier|)
             (|_Time-Interval34590| |instance-of| |Time-Interval|)
             (|_Time-Interval34587| |instance-of| |Time-Interval|)
             (|_Time-Interval34584| |instance-of| |Time-Interval|)
             (|_Entity34576| |instance-of| |Spatial-Entity|)
             (|_Be-Stable34581| |instance-of| |Be-Stable|)
             (|_Be-Supported34580| |instance-of| |Be-Supported|)
             (|_Conduit34579| |instance-of| |Conduit|)
             (|_Bridge34572| |instance-of| |Bridge|)
             (|_Create34578| |instance-of| |Create|)
             (|_Entity34576| |plays| |_Barrier34577|)
             (|_Be-Stable34581| |actions| |_Be-Stable34581|)
             (|_Be-Stable34581| |primitive-actions| |_Be-Stable34581|)
             (|_Be-Stable34581| |time-during| |_Time-Interval34590|)
             (|_Be-Supported34580| |actions| |_Be-Supported34580|)
             (|_Be-Supported34580| |primitive-actions|
              |_Be-Supported34580|)
             (|_Be-Supported34580| |time-during| |_Time-Interval34587|)
             (|_Create34578| |actions| |_Create34578|)
             (|_Create34578| |primitive-actions| |_Create34578|)
             (|_Create34578| |time-during| |_Time-Interval34584|)
             (|_Bridge34572| |is-over| |_Entity34576|)
             (|_Bridge34572| |object-of| |_Be-Stable34581|)
             (|_Bridge34572| |object-of| |_Be-Supported34580|)
             (|_Bridge34572| |plays| |_Conduit34579|)
             (|_Bridge34572| |result-of| |_Create34578|))
            T NIL)))
(|Prosecution| (|_Prosecution498|
                (((|_Prosecution498| |instance-of| |Prosecution|)
                  (|_Time-Interval504| |instance-of| |Time-Interval|)
                  (|_Prosecution498| |actions| |_Prosecution498|)
                  (|_Prosecution498| |primitive-actions|
                   |_Prosecution498|)
                  (|_Prosecution498| |time-during|
                   |_Time-Interval504|))
                 T NIL)))
(|Distance-Constant| (|_Distance-Constant37891|
                      (((|_Distance-Constant37891| |instance-of|
                         |Distance-Constant|))
                       T NIL)))
(|City| (|_City34687| (((|_City34687| |instance-of| |City|)) T NIL)))
(|Nonrenewable-Resource| (|_Nonrenewable-Resource32969|
                          (((|_Nonrenewable-Resource32969|
                             |instance-of| |Nonrenewable-Resource|))
                           T NIL)))
(|EntityProperty| (|_EntityProperty37959|
                   (((|_EntityProperty37959| |instance-of|
                      |EntityProperty|))
                    T NIL)))
(|Drawer| (|_Drawer34186|
           (((|_Time-Interval34191| |instance-of| |Time-Interval|)
             (|_Drawer34186| |instance-of| |Drawer|)
             (|_Create34188| |instance-of| |Create|)
             (|_Create34188| |actions| |_Create34188|)
             (|_Create34188| |primitive-actions| |_Create34188|)
             (|_Create34188| |time-during| |_Time-Interval34191|)
             (|_Drawer34186| |result-of| |_Create34188|))
            T NIL)))
(|State-Value| (|_State-Value37695|
                (((|_State-Value37695| |instance-of| |State-Value|)) T
                 NIL)))
(|Be-Restrained| (|_Be-Restrained281|
                  (((|_Time-Interval285| |instance-of| |Time-Interval|)
                    (|_Be-Restrained281| |instance-of| |Be-Restrained|)
                    (|_Entity284| |instance-of| |Entity|)
                    (|_Be-Restrained281| |actions| |_Be-Restrained281|)
                    (|_Be-Restrained281| |primitive-actions|
                     |_Be-Restrained281|)
                    (|_Be-Restrained281| |time-during|
                     |_Time-Interval285|)
                    (|_Be-Restrained281| |object| |_Entity284|))
                   T NIL)))
(|Longitude-Constant| (|_Longitude-Constant37871|
                       (((|_Longitude-Constant37871| |instance-of|
                          |Longitude-Constant|))
                        T NIL)))
(|Animal| (|_Animal34017|
           (((|_Animal34017| |instance-of| |Animal|)
             (|_Agent-Role34018| |instance-of| |Agent-Role|)
             (|_Animal34017| |capability| |_Agent-Role34018|))
            T NIL)))
(|Categorical| (|_Categorical37933|
                (((|_Categorical37933| |instance-of| |Categorical|)) T
                 NIL)))
(|Piece-of-Wood| (|_Piece-of-Wood34041|
                  (((|_Piece-of-Wood34041| |instance-of|
                     |Piece-of-Wood|)
                    (|_Wood34044| |instance-of| |Wood|)
                    (|_Piece-of-Wood34041| |material| |_Wood34044|))
                   T NIL)))
(|Color-Value| (|_Color-Value37749|
                (((|_Color-Value37749| |instance-of| |Color-Value|)) T
                 NIL)))
(|Interface-Slot| (|_Interface-Slot37969|
                   (((|_Interface-Slot37969| |instance-of|
                      |Interface-Slot|))
                    T NIL)))
(|UoM-Duration| (|_UoM-Duration37466|
                 (((|_UoM-Duration37466| |instance-of| |UoM-Duration|))
                  T NIL)))
(|Piece-of-Metal| (|_Piece-of-Metal34119|
                   (((|_Piece-of-Metal34119| |instance-of|
                      |Piece-of-Metal|)
                     (|_Metal34122| |instance-of| |Metal|)
                     (|_Piece-of-Metal34119| |material| |_Metal34122|))
                    T NIL)))
(|Admit| (|_Admit24435|
          (((|_Container24443| |instance-of| |Container|)
            (|_Move24442| |instance-of| |Move|)
            (|_Shut-Out24440| |instance-of| |Shut-Out|)
            (|_Time-Interval24439| |instance-of| |Time-Interval|)
            (|_Tangible-Entity24438| |instance-of| |Tangible-Entity|)
            (|_Admit24435| |instance-of| |Admit|)
            (|_Tangible-Entity24436| |instance-of| |Tangible-Entity|)
            (|_Tangible-Entity24438| |plays| |_Container24443|)
            (|_Admit24435| |actions| |_Admit24435|)
            (|_Admit24435| |preparatory-event| |_Move24442|)
            (|_Admit24435| |preparatory-event| |_Shut-Out24440|)
            (|_Admit24435| |primitive-actions| |_Admit24435|)
            (|_Admit24435| |time-during| |_Time-Interval24439|)
            (|_Admit24435| |base| |_Tangible-Entity24438|)
            (|_Admit24435| |object| |_Tangible-Entity24436|))
           T NIL)))
(|Increase| (|_Increase24652|
             (((|_Time-Interval24660| |instance-of| |Time-Interval|)
               (|_Property-Value24658| |instance-of| |Property-Value|)
               (|_Increase24652| |instance-of| |Increase|)
               (|_Property-Value24659| |instance-of| |Property-Value|)
               (|_Increase24652| |actions| |_Increase24652|)
               (|_Increase24652| |primitive-actions| |_Increase24652|)
               (|_Increase24652| |time-during| |_Time-Interval24660|)
               (|_Increase24652| |from-value| |_Property-Value24658|)
               (|_Increase24652| |to-value| |_Property-Value24659|))
              T NIL)))
(|UoM-Length| (|_UoM-Length37460|
               (((|_UoM-Length37460| |instance-of| |UoM-Length|)) T
                NIL)))
(|Enter| (|_Enter37188|
          (((|_Portal37215| |instance-of| |Portal|)
            (|_Container37283| |instance-of| |Container|)
            (|_Time-Interval37280| |instance-of| |Time-Interval|)
            (|_Spatial-Entity37272| |instance-of| |Spatial-Entity|)
            (|_Spatial-Entity37271| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity37279| |instance-of| |Tangible-Entity|)
            (|_Tangible-Entity37268| |instance-of| |Tangible-Entity|)
            (|_Move37245| |instance-of| |Move|)
            (|_Admit37244| |instance-of| |Admit|)
            (|_Time-Interval37243| |instance-of| |Time-Interval|)
            (|_Spatial-Entity37211| |instance-of| |Spatial-Entity|)
            (|_Spatial-Entity37210| |instance-of| |Spatial-Entity|)
            (|_Spatial-Entity37212| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity37213| |instance-of| |Tangible-Entity|)
            (|_Be-Contained37242| |instance-of| |Be-Contained|)
            (|_Tangible-Entity37209| |instance-of| |Tangible-Entity|)
            (|_Acceleration-Magnitude-Value37218| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value37217| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value37224| |instance-of| |Length-Value|)
            (|_Duration-Value37216| |instance-of| |Duration-Value|)
            (|_Speed-Value37220| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value37223| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value37241| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value37238| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value37240| |instance-of| |Speed-Value|)
            (|_Speed-Value37239| |instance-of| |Speed-Value|)
            (|_Speed-Value37237| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value37234| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value37236| |instance-of| |Speed-Value|)
            (|_Speed-Value37235| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value37219| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value37233| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value37232| |instance-of| |Length-Value|)
            (|_Speed-Value37231| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value37230| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value37229| |instance-of| |Length-Value|)
            (|_Enter37188| |instance-of| |Enter|)
            (|_Speed-Value37228| |instance-of| |Speed-Value|)
            (|_Spatial-Entity37210| |is-outside|
             |_Tangible-Entity37213|)
            (|_Tangible-Entity37213| |base-of| |_Be-Contained37242|)
            (|_Spatial-Entity37212| |plays| |_Portal37215|)
            (|_Tangible-Entity37213| |encloses| |_Spatial-Entity37211|)
            (|_Tangible-Entity37213| |plays| |_Container37283|)
            (|_Tangible-Entity37213| |has-region|
             |_Spatial-Entity37212|)
            (|_Be-Contained37242| |actions| |_Be-Contained37242|)
            (|_Be-Contained37242| |primitive-actions|
             |_Be-Contained37242|)
            (|_Be-Contained37242| |time-during| |_Time-Interval37280|)
            (|_Be-Contained37242| |destination| |_Spatial-Entity37272|)
            (|_Be-Contained37242| |origin| |_Spatial-Entity37271|)
            (|_Be-Contained37242| |base| |_Tangible-Entity37279|)
            (|_Be-Contained37242| |object| |_Tangible-Entity37268|)
            (|_Enter37188| |actions| |_Enter37188|)
            (|_Enter37188| |preparatory-event| |_Move37245|)
            (|_Enter37188| |preparatory-event| |_Admit37244|)
            (|_Enter37188| |primitive-actions| |_Enter37188|)
            (|_Enter37188| |time-during| |_Time-Interval37243|)
            (|_Enter37188| |destination| |_Spatial-Entity37211|)
            (|_Enter37188| |origin| |_Spatial-Entity37210|)
            (|_Enter37188| |path| |_Spatial-Entity37212|)
            (|_Enter37188| |base| |_Tangible-Entity37213|)
            (|_Enter37188| |object| |_Tangible-Entity37209|)
            (|_Enter37188| |resulting-state| |_Be-Contained37242|)
            (|_Enter37188| |agent| |_Tangible-Entity37209|)
            (|_Enter37188| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value37218|)
            (|_Enter37188| |acceleration|
             |_Acceleration-Vector-Value37217|)
            (|_Enter37188| |distance| |_Length-Value37224|)
            (|_Enter37188| |duration| |_Duration-Value37216|)
            (|_Enter37188| |speed| |_Speed-Value37220|)
            (|_Enter37188| |displacement|
             |_Displacement-Vector-Value37223|)
            (|_Enter37188| |final-speed| |_Speed-Value37241|)
            (|_Enter37188| |final-velocity|
             |_Velocity-Vector-Value37238|)
            (|_Enter37188| |final-x-speed| |_Speed-Value37240|)
            (|_Enter37188| |final-y-speed| |_Speed-Value37239|)
            (|_Enter37188| |initial-speed| |_Speed-Value37237|)
            (|_Enter37188| |initial-velocity|
             |_Velocity-Vector-Value37234|)
            (|_Enter37188| |initial-x-speed| |_Speed-Value37236|)
            (|_Enter37188| |initial-y-speed| |_Speed-Value37235|)
            (|_Enter37188| |velocity| |_Velocity-Vector-Value37219|)
            (|_Enter37188| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value37233|)
            (|_Enter37188| |x-distance| |_Length-Value37232|)
            (|_Enter37188| |x-speed| |_Speed-Value37231|)
            (|_Enter37188| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value37230|)
            (|_Enter37188| |y-distance| |_Length-Value37229|)
            (|_Enter37188| |y-speed| |_Speed-Value37228|))
           T NIL)))
(|Break| (|_Break24827|
          (((|_Time-Interval24838| |instance-of| |Time-Interval|)
            (|_Physical-Object24834| |instance-of| |Physical-Object|)
            (|_Move24832| |instance-of| |Move|)
            (|_Time-Interval24831| |instance-of| |Time-Interval|)
            (|_Tangible-Entity24828| |instance-of| |Physical-Object|)
            (|_Break24827| |instance-of| |Break|)
            (|_Be-Broken24830| |instance-of| |Be-Broken|)
            (|_Be-Broken24830| |actions| |_Be-Broken24830|)
            (|_Be-Broken24830| |primitive-actions| |_Be-Broken24830|)
            (|_Be-Broken24830| |time-during| |_Time-Interval24838|)
            (|_Be-Broken24830| |object| |_Physical-Object24834|)
            (|_Break24827| |actions| |_Break24827|)
            (|_Break24827| |preparatory-event| |_Move24832|)
            (|_Break24827| |primitive-actions| |_Break24827|)
            (|_Break24827| |time-during| |_Time-Interval24831|)
            (|_Break24827| |object| |_Tangible-Entity24828|)
            (|_Break24827| |resulting-state| |_Be-Broken24830|))
           T NIL)))
(|Brighten| (|_Brighten24732|
             (((|_Time-Interval24743| |instance-of| |Time-Interval|)
               (|_Tangible-Entity24742| |instance-of|
                |Tangible-Entity|)
               (|_Brightness-Value24738| |instance-of|
                |Brightness-Value|)
               (|_Brighten24732| |instance-of| |Brighten|)
               (|_Brightness-Value24739| |instance-of|
                |Brightness-Value|)
               (|_Brightness-Value24739| |greater-than|
                |_Brightness-Value24738|)
               (|_Brighten24732| |actions| |_Brighten24732|)
               (|_Brighten24732| |primitive-actions| |_Brighten24732|)
               (|_Brighten24732| |time-during| |_Time-Interval24743|)
               (|_Brighten24732| |base| |_Tangible-Entity24742|)
               (|_Brighten24732| |from-value| |_Brightness-Value24738|)
               (|_Brighten24732| |to-value| |_Brightness-Value24739|))
              T NIL)))
(|Compute-Qualitative-Minimum| (|_Compute-Qualitative-Minimum34956|
                                (((|_Compute-Qualitative-Minimum34956|
                                   |instance-of|
                                   |Compute-Qualitative-Minimum|))
                                 T
                                 NIL)))
(|Analysis| (|_Analysis965|
             (((|_Analysis965| |instance-of| |Analysis|)
               (|_Time-Interval971| |instance-of| |Time-Interval|)
               (|_Analysis965| |actions| |_Analysis965|)
               (|_Analysis965| |primitive-actions| |_Analysis965|)
               (|_Analysis965| |time-during| |_Time-Interval971|))
              T NIL)))
(|Be-Related| (|_Be-Related236|
               (((|_Be-Related236| |instance-of| |Be-Related|)
                 (|_Time-Interval238| |instance-of| |Time-Interval|)
                 (|_Be-Related236| |actions| |_Be-Related236|)
                 (|_Be-Related236| |primitive-actions|
                  |_Be-Related236|)
                 (|_Be-Related236| |time-during| |_Time-Interval238|))
                T NIL)))
(|Recipient-Role| (|_Recipient-Role32978|
                   (((|_Recipient-Role32978| |instance-of|
                      |Recipient-Role|)
                     (|_Entity32980| |instance-of| |Entity|)
                     (|_Recipient-Role32978| |played-by|
                      |_Entity32980|))
                    T NIL)))
(|Connector| (|_Connector33034|
              (((|_Be-Attached-To33037| |instance-of| |Be-Attached-To|)
                (|_Connector33034| |instance-of| |Connector|)
                (|_Tangible-Entity33036| |instance-of|
                 |Tangible-Entity|)
                (|_Connector33034| |in-event| |_Be-Attached-To33037|)
                (|_Connector33034| |played-by|
                 |_Tangible-Entity33036|))
               T NIL)))
(|Compute-Quantitative-Minimum| (|_Compute-Quantitative-Minimum34952|
                                 (((|_Compute-Quantitative-Minimum34952|
                                    |instance-of|
                                    |Compute-Quantitative-Minimum|))
                                  T
                                  NIL)))
(|Province| (|_Province34679|
             (((|_Province34679| |instance-of| |Province|)) T NIL)))
(|Length-Value| (|_Length-Value37729|
                 (((|_Length-Value37729| |instance-of| |Length-Value|))
                  T NIL)))
(|UoM-Angle| (|_UoM-Angle37476|
              (((|_UoM-Angle37476| |instance-of| |UoM-Angle|)) T NIL)))
(|Area-Constant| (|_Area-Constant37915|
                  (((|_Area-Constant37915| |instance-of|
                     |Area-Constant|))
                   T NIL)))
(|Shape-Value| (|_Shape-Value37700|
                (((|_Shape-Value37700| |instance-of| |Shape-Value|)) T
                 NIL)))
(|Height-Constant| (|_Height-Constant37885|
                    (((|_Height-Constant37885| |instance-of|
                       |Height-Constant|))
                     T NIL)))
(|Aggregation-Slot| (|_Aggregation-Slot14|
                     (((|_Aggregation-Slot14| |instance-of|
                        |Aggregation-Slot|))
                      T NIL)))
(|EventProperty| (|_EventProperty37957|
                  (((|_EventProperty37957| |instance-of|
                     |EventProperty|))
                   T NIL)))
(|Yes-No-Viewpoint| (|_Yes-No-Viewpoint34924|
                     (((|_Yes-No-Viewpoint34924| |instance-of|
                        |Yes-No-Viewpoint|))
                      T NIL)))
(|Acceleration-Magnitude-Constant| (|_Acceleration-Magnitude-Constant37923|
                                    (((|_Acceleration-Magnitude-Constant37923|
                                       |instance-of|
                                       |Acceleration-Magnitude-Constant|))
                                     T
                                     NIL)))
(|Orient| (|_Orient10262|
           (((|_Orient10262| |instance-of| |Orient|)
             (|_Time-Interval10268| |instance-of| |Time-Interval|)
             (|_Orient10262| |actions| |_Orient10262|)
             (|_Orient10262| |primitive-actions| |_Orient10262|)
             (|_Orient10262| |time-during| |_Time-Interval10268|))
            T NIL)))
(|Intangible-Entity| (|_Intangible-Entity34728|
                      (((|_Intangible-Entity34728| |instance-of|
                         |Intangible-Entity|))
                       T NIL)))
(|Big-Node| (|_Big-Node34898|
             (((|_Big-Node34898| |instance-of| |Big-Node|)) T NIL)))
(|UoM-Speed| (|_UoM-Speed37436|
              (((|_UoM-Speed37436| |instance-of| |UoM-Speed|)) T NIL)))
(|Disperse| (|_Disperse25265|
             (((|_Move25282| |instance-of| |Move|)
               (|_Time-Interval25279| |instance-of| |Time-Interval|)
               (|_Spatial-Entity25271| |instance-of| |Spatial-Entity|)
               (|_Tangible-Entity25274| |instance-of|
                |Tangible-Entity|)
               (|_Tangible-Entity25273| |instance-of|
                |Tangible-Entity|)
               (|_Leave25276| |instance-of| |Leave|)
               (|_Disperse25265| |instance-of| |Disperse|)
               (|_Leave25275| |instance-of| |Leave|)
               (|_Disperse25265| |actions| |_Disperse25265|)
               (|_Disperse25265| |actions| |_Leave25276|)
               (|_Disperse25265| |actions| |_Leave25275|)
               (|_Disperse25265| |all-subevents| |_Leave25276|)
               (|_Disperse25265| |all-subevents| |_Leave25275|)
               (|_Disperse25265| |preparatory-event| |_Move25282|)
               (|_Disperse25265| |primitive-actions| |_Leave25276|)
               (|_Disperse25265| |primitive-actions| |_Leave25275|)
               (|_Disperse25265| |time-during| |_Time-Interval25279|)
               (|_Disperse25265| |origin| |_Spatial-Entity25271|)
               (|_Disperse25265| |object| |_Tangible-Entity25274|)
               (|_Disperse25265| |object| |_Tangible-Entity25273|)
               (|_Disperse25265| |first-subevent| |_Leave25276|)
               (|_Disperse25265| |first-subevent| |_Leave25275|)
               (|_Disperse25265| |subevent| |_Leave25276|)
               (|_Disperse25265| |subevent| |_Leave25275|))
              T NIL)))
(|Consume| (|_Consume25979|
            (((|_Move25988| |instance-of| |Move|)
              (|_Time-Interval25987| |instance-of| |Time-Interval|)
              (|_Resource25986| |instance-of| |Resource|)
              (|_Consume25979| |instance-of| |Consume|)
              (|_Entity25984| |instance-of| |Spatial-Entity|)
              (|_Entity25984| |plays| |_Resource25986|)
              (|_Consume25979| |actions| |_Consume25979|)
              (|_Consume25979| |preparatory-event| |_Move25988|)
              (|_Consume25979| |primitive-actions| |_Consume25979|)
              (|_Consume25979| |time-during| |_Time-Interval25987|)
              (|_Consume25979| |base| |_Resource25986|)
              (|_Consume25979| |object| |_Entity25984|))
             T NIL)))
(|UoM-Temperature| (|_UoM-Temperature37434|
                    (((|_UoM-Temperature37434| |instance-of|
                       |UoM-Temperature|))
                     T NIL)))
(|Force-Scale| (|_Force-Scale37667|
                (((|_Force-Scale37667| |instance-of| |Force-Scale|)
                  (|_Number37669| |instance-of| |Number|)
                  (|_Force-Scale37667| |number-of-elements|
                   |_Number37669|))
                 T NIL)))
(|UoM-Worth| (|_UoM-Worth37424|
              (((|_UoM-Worth37424| |instance-of| |UoM-Worth|)) T NIL)))
(|Height-Scale| (|_Height-Scale37587|
                 (((|_Height-Scale37587| |instance-of| |Height-Scale|)
                   (|_Number37589| |instance-of| |Number|)
                   (|_Height-Scale37587| |number-of-elements|
                    |_Number37589|))
                  T NIL)))
(|Speak| (|_Speak25609|
          (((|_Move25620| |instance-of| |Move|)
            (|_Time-Interval25619| |instance-of| |Time-Interval|)
            (|_Message25617| |instance-of| |Message|)
            (|_Message25617| |instance-of| |Spatial-Entity|)
            (|_Sound25618| |instance-of| |Sound|)
            (|_Speak25609| |instance-of| |Speak|)
            (|_Tangible-Entity25615| |instance-of| |Tangible-Entity|)
            (|_Speak25609| |actions| |_Speak25609|)
            (|_Speak25609| |preparatory-event| |_Move25620|)
            (|_Speak25609| |primitive-actions| |_Speak25609|)
            (|_Speak25609| |time-during| |_Time-Interval25619|)
            (|_Speak25609| |object| |_Message25617|)
            (|_Speak25609| |result| |_Sound25618|)
            (|_Speak25609| |agent| |_Tangible-Entity25615|))
           T NIL)))
(|Pressure-Scale| (|_Pressure-Scale37552|
                   (((|_Pressure-Scale37552| |instance-of|
                      |Pressure-Scale|)
                     (|_Number37554| |instance-of| |Number|)
                     (|_Pressure-Scale37552| |number-of-elements|
                      |_Number37554|))
                    T NIL)))
(|Quantity-Value| (|_Quantity-Value37708|
                   (((|_Quantity-Value37708| |instance-of|
                      |Quantity-Value|))
                    T NIL)))
(|Force-Magnitude-Value| (|_Force-Magnitude-Value37731|
                          (((|_Force-Magnitude-Value37731|
                             |instance-of| |Force-Magnitude-Value|))
                           T NIL)))
(|Bicycle| (|_Bicycle34598|
            (((|_Bicycle34598| |instance-of| |Bicycle|)
              (|_Vehicle34600| |instance-of| |Vehicle|)
              (|_Bicycle34598| |purpose| |_Vehicle34600|))
             T NIL)))
(|Be-Unavailable| (|_Be-Unavailable32|
                   (((|_Time-Interval36| |instance-of| |Time-Interval|)
                     (|_Be-Unavailable32| |instance-of|
                      |Be-Unavailable|)
                     (|_Resource35| |instance-of| |Resource|)
                     (|_Be-Unavailable32| |actions|
                      |_Be-Unavailable32|)
                     (|_Be-Unavailable32| |primitive-actions|
                      |_Be-Unavailable32|)
                     (|_Be-Unavailable32| |time-during|
                      |_Time-Interval36|)
                     (|_Be-Unavailable32| |object| |_Resource35|))
                    T NIL)))
(|Event| (|_Event22|
          (((|_Event22| |instance-of| |Event|)
            (|_Time-Interval24| |instance-of| |Time-Interval|)
            (|_Event22| |actions| |_Event22|)
            (|_Event22| |primitive-actions| |_Event22|)
            (|_Event22| |time-during| |_Time-Interval24|))
           T NIL)))
(|Table| (|_Table34631|
          (((|_Table34631| |instance-of| |Table|)) T NIL)))
(|Question| (|_Question32981|
             (((|_Question32981| |instance-of| |Question|)) T NIL)))
(|Rock| (|_Rock34625| (((|_Rock34625| |instance-of| |Rock|)) T NIL)))
(|Activate| (|_Activate24550|
             (((|_Time-Interval24563| |instance-of| |Time-Interval|)
               (|_Entity24559| |instance-of| |Entity|)
               (|_Move24557| |instance-of| |Move|)
               (|_Deactivate24555| |instance-of| |Deactivate|)
               (|_Time-Interval24554| |instance-of| |Time-Interval|)
               (|_Entity24551| |instance-of| |Spatial-Entity|)
               (|_Activate24550| |instance-of| |Activate|)
               (|_Be-Activated24553| |instance-of| |Be-Activated|)
               (|_Be-Activated24553| |actions| |_Be-Activated24553|)
               (|_Be-Activated24553| |primitive-actions|
                |_Be-Activated24553|)
               (|_Be-Activated24553| |time-during|
                |_Time-Interval24563|)
               (|_Be-Activated24553| |object| |_Entity24559|)
               (|_Activate24550| |actions| |_Activate24550|)
               (|_Activate24550| |preparatory-event| |_Move24557|)
               (|_Activate24550| |preparatory-event|
                |_Deactivate24555|)
               (|_Activate24550| |primitive-actions| |_Activate24550|)
               (|_Activate24550| |time-during| |_Time-Interval24554|)
               (|_Activate24550| |object| |_Entity24551|)
               (|_Activate24550| |resulting-state|
                |_Be-Activated24553|))
              T NIL)))
(|Fluid-Substance| (|_Fluid-Substance33858|
                    (((|_Fluid-Substance33858| |instance-of|
                       |Fluid-Substance|))
                     T NIL)))
(|Communicate| (|_Communicate26004|
                (((|_Move26172| |instance-of| |Move|)
                  (|_Time-Interval26171| |instance-of| |Time-Interval|)
                  (|_Signal26134| |instance-of| |Signal|)
                  (|_Move26121| |instance-of| |Move|)
                  (|_Time-Interval26120| |instance-of| |Time-Interval|)
                  (|_Time-Interval26082| |instance-of| |Time-Interval|)
                  (|_Tangible-Entity26081| |instance-of|
                   |Living-Entity|)
                  (|_Entity26077| |instance-of| |Entity|)
                  (|_Move26075| |instance-of| |Move|)
                  (|_Time-Interval26074| |instance-of| |Time-Interval|)
                  (|_Be-Known26073| |instance-of| |Be-Known|)
                  (|_Move26066| |instance-of| |Move|)
                  (|_Time-Interval26065| |instance-of| |Time-Interval|)
                  (|_Tangible-Entity26064| |instance-of|
                   |Tangible-Entity|)
                  (|_Tangible-Entity26057| |instance-of|
                   |Tangible-Entity|)
                  (|_Move26055| |instance-of| |Move|)
                  (|_Time-Interval26054| |instance-of| |Time-Interval|)
                  (|_Tangible-Entity26047| |instance-of|
                   |Tangible-Entity|)
                  (|_Time-Interval26028| |instance-of| |Time-Interval|)
                  (|_Tangible-Entity26027| |instance-of|
                   |Living-Entity|)
                  (|_Entity26023| |instance-of| |Entity|)
                  (|_Move26021| |instance-of| |Move|)
                  (|_Sense26020| |instance-of| |Sense|)
                  (|_Transmit26019| |instance-of| |Transmit|)
                  (|_Embody26018| |instance-of| |Embody|)
                  (|_Time-Interval26017| |instance-of| |Time-Interval|)
                  (|_Message26016| |instance-of| |Message|)
                  (|_Message26016| |instance-of| |Spatial-Entity|)
                  (|_Information26011| |instance-of| |Information|)
                  (|_Information26011| |instance-of| |Spatial-Entity|)
                  (|_Tangible-Entity26009| |instance-of|
                   |Tangible-Entity|)
                  (|_Interpret26015| |instance-of| |Interpret|)
                  (|_Convey26014| |instance-of| |Convey|)
                  (|_Express26013| |instance-of| |Express|)
                  (|_Communicate26004| |instance-of| |Communicate|)
                  (|_Be-Known26012| |instance-of| |Be-Known|)
                  (|_Tangible-Entity26064| |destination-of|
                   |_Move26172|)
                  (|_Sense26020| |actions-of| |_Sense26020|)
                  (|_Sense26020| |preparatory-event| |_Move26172|)
                  (|_Sense26020| |primitive-actions-of| |_Sense26020|)
                  (|_Sense26020| |time-during| |_Time-Interval26171|)
                  (|_Sense26020| |experiencer| |_Tangible-Entity26009|)
                  (|_Sense26020| |object| |_Tangible-Entity26064|)
                  (|_Sense26020| |result| |_Message26016|)
                  (|_Sense26020| |agent| |_Tangible-Entity26009|)
                  (|_Tangible-Entity26064| |object-of|
                   |_Transmit26019|)
                  (|_Tangible-Entity26064| |plays| |_Signal26134|)
                  (|_Embody26018| |actions-of| |_Embody26018|)
                  (|_Embody26018| |preparatory-event| |_Move26121|)
                  (|_Embody26018| |primitive-actions-of|
                   |_Embody26018|)
                  (|_Embody26018| |time-during| |_Time-Interval26120|)
                  (|_Embody26018| |object| |_Message26016|)
                  (|_Embody26018| |result| |_Tangible-Entity26064|)
                  (|_Embody26018| |next-event| |_Transmit26019|)
                  (|_Embody26018| |agent| |_Tangible-Entity26057|)
                  (|_Be-Known26073| |actions| |_Be-Known26073|)
                  (|_Be-Known26073| |primitive-actions|
                   |_Be-Known26073|)
                  (|_Be-Known26073| |time-during|
                   |_Time-Interval26082|)
                  (|_Be-Known26073| |experiencer|
                   |_Tangible-Entity26081|)
                  (|_Be-Known26073| |object| |_Entity26077|)
                  (|_Interpret26015| |actions-of| |_Interpret26015|)
                  (|_Interpret26015| |preparatory-event| |_Move26075|)
                  (|_Interpret26015| |primitive-actions-of|
                   |_Interpret26015|)
                  (|_Interpret26015| |time-during|
                   |_Time-Interval26074|)
                  (|_Interpret26015| |object| |_Message26016|)
                  (|_Interpret26015| |result| |_Information26011|)
                  (|_Interpret26015| |resulting-state|
                   |_Be-Known26073|)
                  (|_Interpret26015| |agent| |_Tangible-Entity26009|)
                  (|_Convey26014| |actions| |_Sense26020|)
                  (|_Convey26014| |actions| |_Transmit26019|)
                  (|_Convey26014| |actions| |_Embody26018|)
                  (|_Convey26014| |actions-of| |_Convey26014|)
                  (|_Convey26014| |all-subevents| |_Sense26020|)
                  (|_Convey26014| |all-subevents| |_Transmit26019|)
                  (|_Convey26014| |all-subevents| |_Embody26018|)
                  (|_Convey26014| |preparatory-event| |_Move26066|)
                  (|_Convey26014| |primitive-actions| |_Sense26020|)
                  (|_Convey26014| |primitive-actions| |_Transmit26019|)
                  (|_Convey26014| |primitive-actions| |_Embody26018|)
                  (|_Convey26014| |time-during| |_Time-Interval26065|)
                  (|_Convey26014| |base| |_Tangible-Entity26064|)
                  (|_Convey26014| |object| |_Message26016|)
                  (|_Convey26014| |recipient| |_Tangible-Entity26009|)
                  (|_Convey26014| |first-subevent| |_Embody26018|)
                  (|_Convey26014| |next-event| |_Interpret26015|)
                  (|_Convey26014| |subevent| |_Sense26020|)
                  (|_Convey26014| |subevent| |_Transmit26019|)
                  (|_Convey26014| |subevent| |_Embody26018|)
                  (|_Convey26014| |agent| |_Tangible-Entity26057|)
                  (|_Express26013| |actions-of| |_Express26013|)
                  (|_Express26013| |preparatory-event| |_Move26055|)
                  (|_Express26013| |primitive-actions-of|
                   |_Express26013|)
                  (|_Express26013| |time-during| |_Time-Interval26054|)
                  (|_Express26013| |object| |_Information26011|)
                  (|_Express26013| |result| |_Message26016|)
                  (|_Express26013| |next-event| |_Convey26014|)
                  (|_Express26013| |agent| |_Tangible-Entity26047|)
                  (|_Be-Known26012| |actions| |_Be-Known26012|)
                  (|_Be-Known26012| |primitive-actions|
                   |_Be-Known26012|)
                  (|_Be-Known26012| |time-during|
                   |_Time-Interval26028|)
                  (|_Be-Known26012| |experiencer|
                   |_Tangible-Entity26027|)
                  (|_Be-Known26012| |object| |_Entity26023|)
                  (|_Communicate26004| |actions| |_Communicate26004|)
                  (|_Communicate26004| |actions| |_Interpret26015|)
                  (|_Communicate26004| |actions| |_Convey26014|)
                  (|_Communicate26004| |actions| |_Sense26020|)
                  (|_Communicate26004| |actions| |_Transmit26019|)
                  (|_Communicate26004| |actions| |_Embody26018|)
                  (|_Communicate26004| |actions| |_Express26013|)
                  (|_Communicate26004| |all-subevents| |_Sense26020|)
                  (|_Communicate26004| |all-subevents|
                   |_Transmit26019|)
                  (|_Communicate26004| |all-subevents| |_Embody26018|)
                  (|_Communicate26004| |all-subevents|
                   |_Interpret26015|)
                  (|_Communicate26004| |all-subevents| |_Convey26014|)
                  (|_Communicate26004| |all-subevents| |_Express26013|)
                  (|_Communicate26004| |preparatory-event|
                   |_Move26021|)
                  (|_Communicate26004| |primitive-actions|
                   |_Interpret26015|)
                  (|_Communicate26004| |primitive-actions|
                   |_Sense26020|)
                  (|_Communicate26004| |primitive-actions|
                   |_Transmit26019|)
                  (|_Communicate26004| |primitive-actions|
                   |_Embody26018|)
                  (|_Communicate26004| |primitive-actions|
                   |_Express26013|)
                  (|_Communicate26004| |time-during|
                   |_Time-Interval26017|)
                  (|_Communicate26004| |base| |_Message26016|)
                  (|_Communicate26004| |object| |_Information26011|)
                  (|_Communicate26004| |recipient|
                   |_Tangible-Entity26009|)
                  (|_Communicate26004| |first-subevent|
                   |_Express26013|)
                  (|_Communicate26004| |subevent| |_Interpret26015|)
                  (|_Communicate26004| |subevent| |_Convey26014|)
                  (|_Communicate26004| |subevent| |_Express26013|)
                  (|_Communicate26004| |resulting-state|
                   |_Be-Known26012|))
                 T NIL)))
(|Chemical-Energy| (|_Chemical-Energy34669|
                    (((|_Chemical-Energy34669| |instance-of|
                       |Chemical-Energy|))
                     T NIL)))
(|Detailed-Instance| (|_Detailed-Instance34976|
                      (((|_Detailed-Instance34976| |instance-of|
                         |Detailed-Instance|))
                       T NIL)))
(|Discourse| (|_Discourse803|
              (((|_Discourse803| |instance-of| |Discourse|)
                (|_Time-Interval809| |instance-of| |Time-Interval|)
                (|_Discourse803| |actions| |_Discourse803|)
                (|_Discourse803| |primitive-actions| |_Discourse803|)
                (|_Discourse803| |time-during| |_Time-Interval809|))
               T NIL)))
(|Move-Apart| (|_Move-Apart23502|
               (((|_Move23519| |instance-of| |Move|)
                 (|_Time-Interval23516| |instance-of| |Time-Interval|)
                 (|_Spatial-Entity23508| |instance-of|
                  |Spatial-Entity|)
                 (|_Tangible-Entity23511| |instance-of|
                  |Tangible-Entity|)
                 (|_Tangible-Entity23510| |instance-of|
                  |Tangible-Entity|)
                 (|_Move23513| |instance-of| |Move|)
                 (|_Move-Apart23502| |instance-of| |Move-Apart|)
                 (|_Move23512| |instance-of| |Move|)
                 (|_Move-Apart23502| |actions| |_Move-Apart23502|)
                 (|_Move-Apart23502| |actions| |_Move23513|)
                 (|_Move-Apart23502| |actions| |_Move23512|)
                 (|_Move-Apart23502| |all-subevents| |_Move23513|)
                 (|_Move-Apart23502| |all-subevents| |_Move23512|)
                 (|_Move-Apart23502| |preparatory-event| |_Move23519|)
                 (|_Move-Apart23502| |primitive-actions| |_Move23513|)
                 (|_Move-Apart23502| |primitive-actions| |_Move23512|)
                 (|_Move-Apart23502| |time-during|
                  |_Time-Interval23516|)
                 (|_Move-Apart23502| |origin| |_Spatial-Entity23508|)
                 (|_Move-Apart23502| |object| |_Tangible-Entity23511|)
                 (|_Move-Apart23502| |object| |_Tangible-Entity23510|)
                 (|_Move-Apart23502| |first-subevent| |_Move23513|)
                 (|_Move-Apart23502| |first-subevent| |_Move23512|)
                 (|_Move-Apart23502| |subevent| |_Move23513|)
                 (|_Move-Apart23502| |subevent| |_Move23512|))
                T NIL)))
(|Message| (|_Message34740|
            (((|_Language34742| |instance-of| |Language|)
              (|_Message34740| |instance-of| |Message|)
              (|_Thing34741| |instance-of| |Thing|)
              (|_Message34740| |information-language| |_Language34742|)
              (|_Message34740| |information-content| |_Thing34741|))
             T NIL)))
(|Frequency-Scale| (|_Frequency-Scale37592|
                    (((|_Frequency-Scale37592| |instance-of|
                       |Frequency-Scale|)
                      (|_Number37594| |instance-of| |Number|)
                      (|_Frequency-Scale37592| |number-of-elements|
                       |_Number37594|))
                     T NIL)))
(|Unauthorized| (|_Unauthorized34982|
                 (((|_Unauthorized34982| |instance-of| |Unauthorized|))
                  T NIL)))
(|Slot-Value-Viewpoint| (|_Slot-Value-Viewpoint34926|
                         (((|_Slot-Value-Viewpoint34926| |instance-of|
                            |Slot-Value-Viewpoint|))
                          T NIL)))
(|Read| (|_Read25509|
         (((|_Move25519| |instance-of| |Move|)
           (|_Time-Interval25518| |instance-of| |Time-Interval|)
           (|_Tangible-Entity25517| |instance-of| |Tangible-Entity|)
           (|_Physical-Document25514| |instance-of|
            |Physical-Document|)
           (|_Read25509| |instance-of| |Read|)
           (|_Message25516| |instance-of| |Message|)
           (|_Read25509| |actions| |_Read25509|)
           (|_Read25509| |preparatory-event| |_Move25519|)
           (|_Read25509| |primitive-actions| |_Read25509|)
           (|_Read25509| |time-during| |_Time-Interval25518|)
           (|_Read25509| |experiencer| |_Tangible-Entity25517|)
           (|_Read25509| |object| |_Physical-Document25514|)
           (|_Read25509| |result| |_Message25516|))
          T NIL)))
(|Supper| (|_Supper579|
           (((|_Time-Interval586| |instance-of| |Time-Interval|)
             (|_Supper579| |instance-of| |Supper|)
             (|_Eat585| |instance-of| |Eat|)
             (|_Supper579| |actions| |_Supper579|)
             (|_Supper579| |actions| |_Eat585|)
             (|_Supper579| |all-subevents| |_Eat585|)
             (|_Supper579| |primitive-actions| |_Eat585|)
             (|_Supper579| |time-during| |_Time-Interval586|)
             (|_Supper579| |subevent| |_Eat585|))
            T NIL)))
(|Number-Field| (|_Number-Field34773|
                 (((|_Language34776| |instance-of| |Language|)
                   (|_Number-Field34773| |instance-of| |Number-Field|)
                   (|_Number34775| |instance-of| |Number|)
                   (|_Number-Field34773| |information-language|
                    |_Language34776|)
                   (|_Number-Field34773| |information-content|
                    |_Number34775|))
                  T NIL)))
(|Preparatory-Event| (|_Preparatory-Event34908|
                      (((|_Preparatory-Event34908| |instance-of|
                         |Preparatory-Event|))
                       T NIL)))
(|Energy| (|_Energy34639|
           (((|_Energy34639| |instance-of| |Energy|)) T NIL)))
(|Arithmetic-Method| (|_Arithmetic-Method34964|
                      (((|_Arithmetic-Method34964| |instance-of|
                         |Arithmetic-Method|))
                       T NIL)))
(|Wood| (|_Wood33639|
         (((|_Categorical33644| |instance-of| |Categorical|)
           (|_Wood33639| |instance-of| |Wood|)
           (|_State-Value33643| |instance-of| |State-Value|)
           (|_State-Value33643| |categorical-value|
            |_Categorical33644|)
           (|_Wood33639| |physical-state| |_State-Value33643|))
          T NIL)))
(|Direction-Scale| (|_Direction-Scale37607|
                    (((|_Direction-Scale37607| |instance-of|
                       |Direction-Scale|)
                      (|_Number37609| |instance-of| |Number|)
                      (|_Direction-Scale37607| |number-of-elements|
                       |_Number37609|))
                     T NIL)))
(|Follow| (|_Follow35298|
           (((|_Time-Interval35346| |instance-of| |Time-Interval|)
             (|_Spatial-Entity35318| |instance-of| |Spatial-Entity|)
             (|_Spatial-Entity35319| |instance-of| |Spatial-Entity|)
             (|_Entity35320| |instance-of| |Entity|)
             (|_Tangible-Entity35317| |instance-of| |Tangible-Entity|)
             (|_Acceleration-Magnitude-Value35323| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Acceleration-Vector-Value35322| |instance-of|
              |Acceleration-Vector-Value|)
             (|_Length-Value35329| |instance-of| |Length-Value|)
             (|_Duration-Value35321| |instance-of| |Duration-Value|)
             (|_Speed-Value35325| |instance-of| |Speed-Value|)
             (|_Displacement-Vector-Value35328| |instance-of|
              |Displacement-Vector-Value|)
             (|_Speed-Value35345| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value35342| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value35344| |instance-of| |Speed-Value|)
             (|_Speed-Value35343| |instance-of| |Speed-Value|)
             (|_Speed-Value35341| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value35338| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value35340| |instance-of| |Speed-Value|)
             (|_Speed-Value35339| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value35324| |instance-of|
              |Velocity-Vector-Value|)
             (|_Acceleration-Magnitude-Value35337| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value35336| |instance-of| |Length-Value|)
             (|_Speed-Value35335| |instance-of| |Speed-Value|)
             (|_Acceleration-Magnitude-Value35334| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value35333| |instance-of| |Length-Value|)
             (|_Follow35298| |instance-of| |Follow|)
             (|_Speed-Value35332| |instance-of| |Speed-Value|)
             (|_Follow35298| |actions| |_Follow35298|)
             (|_Follow35298| |primitive-actions| |_Follow35298|)
             (|_Follow35298| |time-during| |_Time-Interval35346|)
             (|_Follow35298| |destination| |_Spatial-Entity35318|)
             (|_Follow35298| |path| |_Spatial-Entity35319|)
             (|_Follow35298| |base| |_Entity35320|)
             (|_Follow35298| |object| |_Tangible-Entity35317|)
             (|_Follow35298| |agent| |_Tangible-Entity35317|)
             (|_Follow35298| |acceleration-magnitude|
              |_Acceleration-Magnitude-Value35323|)
             (|_Follow35298| |acceleration|
              |_Acceleration-Vector-Value35322|)
             (|_Follow35298| |distance| |_Length-Value35329|)
             (|_Follow35298| |duration| |_Duration-Value35321|)
             (|_Follow35298| |speed| |_Speed-Value35325|)
             (|_Follow35298| |displacement|
              |_Displacement-Vector-Value35328|)
             (|_Follow35298| |final-speed| |_Speed-Value35345|)
             (|_Follow35298| |final-velocity|
              |_Velocity-Vector-Value35342|)
             (|_Follow35298| |final-x-speed| |_Speed-Value35344|)
             (|_Follow35298| |final-y-speed| |_Speed-Value35343|)
             (|_Follow35298| |initial-speed| |_Speed-Value35341|)
             (|_Follow35298| |initial-velocity|
              |_Velocity-Vector-Value35338|)
             (|_Follow35298| |initial-x-speed| |_Speed-Value35340|)
             (|_Follow35298| |initial-y-speed| |_Speed-Value35339|)
             (|_Follow35298| |velocity| |_Velocity-Vector-Value35324|)
             (|_Follow35298| |x-acceleration-magnitude|
              |_Acceleration-Magnitude-Value35337|)
             (|_Follow35298| |x-distance| |_Length-Value35336|)
             (|_Follow35298| |x-speed| |_Speed-Value35335|)
             (|_Follow35298| |y-acceleration-magnitude|
              |_Acceleration-Magnitude-Value35334|)
             (|_Follow35298| |y-distance| |_Length-Value35333|)
             (|_Follow35298| |y-speed| |_Speed-Value35332|))
            T NIL)))
(|Property-Value| (|_Property-Value37674|
                   (((|_Property-Value37674| |instance-of|
                      |Property-Value|))
                    T NIL)))
(|Print| (|_Print25667|
          (((|_Move25680| |instance-of| |Move|)
            (|_Time-Interval25679| |instance-of| |Time-Interval|)
            (|_Document25676| |instance-of| |Document|)
            (|_Document25676| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity25674| |instance-of| |Tangible-Entity|)
            (|_Physical-Mark25678| |instance-of| |Physical-Mark|)
            (|_Print25667| |instance-of| |Print|)
            (|_Physical-Document25677| |instance-of|
             |Physical-Document|)
            (|_Print25667| |actions| |_Print25667|)
            (|_Print25667| |preparatory-event| |_Move25680|)
            (|_Print25667| |primitive-actions| |_Print25667|)
            (|_Print25667| |time-during| |_Time-Interval25679|)
            (|_Print25667| |object| |_Document25676|)
            (|_Print25667| |raw-material| |_Tangible-Entity25674|)
            (|_Print25667| |result| |_Physical-Mark25678|)
            (|_Print25667| |result| |_Physical-Document25677|))
           T NIL)))
(|Building-Complex| (|_Building-Complex34556|
                     (((|_Time-Interval34569| |instance-of|
                        |Time-Interval|)
                       (|_Time-Interval34566| |instance-of|
                        |Time-Interval|)
                       (|_Time-Interval34563| |instance-of|
                        |Time-Interval|)
                       (|_Be-Stable34560| |instance-of| |Be-Stable|)
                       (|_Be-Supported34559| |instance-of|
                        |Be-Supported|)
                       (|_Building-Complex34556| |instance-of|
                        |Building-Complex|)
                       (|_Create34558| |instance-of| |Create|)
                       (|_Be-Stable34560| |actions| |_Be-Stable34560|)
                       (|_Be-Stable34560| |primitive-actions|
                        |_Be-Stable34560|)
                       (|_Be-Stable34560| |time-during|
                        |_Time-Interval34569|)
                       (|_Be-Supported34559| |actions|
                        |_Be-Supported34559|)
                       (|_Be-Supported34559| |primitive-actions|
                        |_Be-Supported34559|)
                       (|_Be-Supported34559| |time-during|
                        |_Time-Interval34566|)
                       (|_Create34558| |actions| |_Create34558|)
                       (|_Create34558| |primitive-actions|
                        |_Create34558|)
                       (|_Create34558| |time-during|
                        |_Time-Interval34563|)
                       (|_Building-Complex34556| |object-of|
                        |_Be-Stable34560|)
                       (|_Building-Complex34556| |object-of|
                        |_Be-Supported34559|)
                       (|_Building-Complex34556| |result-of|
                        |_Create34558|))
                      T NIL)))
(|Piece-of-Rubber| (|_Piece-of-Rubber34080|
                    (((|_Piece-of-Rubber34080| |instance-of|
                       |Piece-of-Rubber|)
                      (|_Rubber34083| |instance-of| |Rubber|)
                      (|_Piece-of-Rubber34080| |material|
                       |_Rubber34083|))
                     T NIL)))
(|Ensemble| (|_Ensemble34882|
             (((|_Ensemble34882| |instance-of| |Ensemble|)
               (|_Number34884| |instance-of| |Number|)
               (|_Ensemble34882| |number-of-elements| |_Number34884|))
              T NIL)))
(|Food| (|_Food33018|
         (((|_Feed33020| |instance-of| |Feed|)
           (|_Food33018| |instance-of| |Food|)
           (|_Tangible-Entity33019| |instance-of| |Tangible-Entity|)
           (|_Food33018| |in-event| |_Feed33020|)
           (|_Food33018| |played-by| |_Tangible-Entity33019|))
          T NIL)))
(|Property-Vector-Value| (|_Property-Vector-Value37763|
                          (((|_Property-Vector-Value37763|
                             |instance-of| |Property-Vector-Value|))
                           T NIL)))
(|Slide| (|_Slide10658|
          (((|_Move10701| |instance-of| |Move|)
            (|_Time-Interval10700| |instance-of| |Time-Interval|)
            (|_Tangible-Entity10673| |instance-of| |Tangible-Entity|)
            (|_Tangible-Entity10685| |instance-of| |Tangible-Entity|)
            (|_Acceleration-Magnitude-Value10676| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value10675| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value10682| |instance-of| |Length-Value|)
            (|_Duration-Value10674| |instance-of| |Duration-Value|)
            (|_Speed-Value10678| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value10681| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value10699| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value10696| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value10698| |instance-of| |Speed-Value|)
            (|_Speed-Value10697| |instance-of| |Speed-Value|)
            (|_Speed-Value10695| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value10692| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value10694| |instance-of| |Speed-Value|)
            (|_Speed-Value10693| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value10677| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value10691| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value10690| |instance-of| |Length-Value|)
            (|_Speed-Value10689| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value10688| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value10687| |instance-of| |Length-Value|)
            (|_Slide10658| |instance-of| |Slide|)
            (|_Speed-Value10686| |instance-of| |Speed-Value|)
            (|_Slide10658| |actions| |_Slide10658|)
            (|_Slide10658| |preparatory-event| |_Move10701|)
            (|_Slide10658| |primitive-actions| |_Slide10658|)
            (|_Slide10658| |time-during| |_Time-Interval10700|)
            (|_Slide10658| |path| |_Tangible-Entity10673|)
            (|_Slide10658| |object| |_Tangible-Entity10685|)
            (|_Slide10658| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value10676|)
            (|_Slide10658| |acceleration|
             |_Acceleration-Vector-Value10675|)
            (|_Slide10658| |distance| |_Length-Value10682|)
            (|_Slide10658| |duration| |_Duration-Value10674|)
            (|_Slide10658| |speed| |_Speed-Value10678|)
            (|_Slide10658| |displacement|
             |_Displacement-Vector-Value10681|)
            (|_Slide10658| |final-speed| |_Speed-Value10699|)
            (|_Slide10658| |final-velocity|
             |_Velocity-Vector-Value10696|)
            (|_Slide10658| |final-x-speed| |_Speed-Value10698|)
            (|_Slide10658| |final-y-speed| |_Speed-Value10697|)
            (|_Slide10658| |initial-speed| |_Speed-Value10695|)
            (|_Slide10658| |initial-velocity|
             |_Velocity-Vector-Value10692|)
            (|_Slide10658| |initial-x-speed| |_Speed-Value10694|)
            (|_Slide10658| |initial-y-speed| |_Speed-Value10693|)
            (|_Slide10658| |velocity| |_Velocity-Vector-Value10677|)
            (|_Slide10658| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value10691|)
            (|_Slide10658| |x-distance| |_Length-Value10690|)
            (|_Slide10658| |x-speed| |_Speed-Value10689|)
            (|_Slide10658| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value10688|)
            (|_Slide10658| |y-distance| |_Length-Value10687|)
            (|_Slide10658| |y-speed| |_Speed-Value10686|))
           T NIL)))
(|Take-In| (|_Take-In20197|
            (((|_Portal20224| |instance-of| |Portal|)
              (|_Container20219| |instance-of| |Container|)
              (|_Time-Interval20276| |instance-of| |Time-Interval|)
              (|_Spatial-Entity20271| |instance-of| |Spatial-Entity|)
              (|_Spatial-Entity20270| |instance-of| |Spatial-Entity|)
              (|_Tangible-Entity20275| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity20268| |instance-of| |Tangible-Entity|)
              (|_Move20255| |instance-of| |Move|)
              (|_Move20254| |instance-of| |Move|)
              (|_Admit20253| |instance-of| |Admit|)
              (|_Time-Interval20252| |instance-of| |Time-Interval|)
              (|_Spatial-Entity20221| |instance-of| |Spatial-Entity|)
              (|_Spatial-Entity20220| |instance-of| |Spatial-Entity|)
              (|_Spatial-Entity20222| |instance-of| |Spatial-Entity|)
              (|_Tangible-Entity20236| |instance-of| |Tangible-Entity|)
              (|_Be-Contained20251| |instance-of| |Be-Contained|)
              (|_Tangible-Entity20218| |instance-of| |Tangible-Entity|)
              (|_Acceleration-Magnitude-Value20227| |instance-of|
               |Acceleration-Magnitude-Value|)
              (|_Acceleration-Vector-Value20226| |instance-of|
               |Acceleration-Vector-Value|)
              (|_Length-Value20233| |instance-of| |Length-Value|)
              (|_Duration-Value20225| |instance-of| |Duration-Value|)
              (|_Speed-Value20229| |instance-of| |Speed-Value|)
              (|_Displacement-Vector-Value20232| |instance-of|
               |Displacement-Vector-Value|)
              (|_Speed-Value20250| |instance-of| |Speed-Value|)
              (|_Velocity-Vector-Value20247| |instance-of|
               |Velocity-Vector-Value|)
              (|_Speed-Value20249| |instance-of| |Speed-Value|)
              (|_Speed-Value20248| |instance-of| |Speed-Value|)
              (|_Speed-Value20246| |instance-of| |Speed-Value|)
              (|_Velocity-Vector-Value20243| |instance-of|
               |Velocity-Vector-Value|)
              (|_Speed-Value20245| |instance-of| |Speed-Value|)
              (|_Speed-Value20244| |instance-of| |Speed-Value|)
              (|_Velocity-Vector-Value20228| |instance-of|
               |Velocity-Vector-Value|)
              (|_Acceleration-Magnitude-Value20242| |instance-of|
               |Acceleration-Magnitude-Value|)
              (|_Length-Value20241| |instance-of| |Length-Value|)
              (|_Speed-Value20240| |instance-of| |Speed-Value|)
              (|_Acceleration-Magnitude-Value20239| |instance-of|
               |Acceleration-Magnitude-Value|)
              (|_Length-Value20238| |instance-of| |Length-Value|)
              (|_Take-In20197| |instance-of| |Take-In|)
              (|_Speed-Value20237| |instance-of| |Speed-Value|)
              (|_Spatial-Entity20220| |is-outside|
               |_Tangible-Entity20218|)
              (|_Tangible-Entity20218| |base-of| |_Be-Contained20251|)
              (|_Spatial-Entity20222| |plays| |_Portal20224|)
              (|_Tangible-Entity20218| |encloses|
               |_Spatial-Entity20221|)
              (|_Tangible-Entity20218| |plays| |_Container20219|)
              (|_Tangible-Entity20218| |has-region|
               |_Spatial-Entity20222|)
              (|_Be-Contained20251| |actions| |_Be-Contained20251|)
              (|_Be-Contained20251| |primitive-actions|
               |_Be-Contained20251|)
              (|_Be-Contained20251| |time-during|
               |_Time-Interval20276|)
              (|_Be-Contained20251| |destination|
               |_Spatial-Entity20271|)
              (|_Be-Contained20251| |origin| |_Spatial-Entity20270|)
              (|_Be-Contained20251| |base| |_Tangible-Entity20275|)
              (|_Be-Contained20251| |object| |_Tangible-Entity20268|)
              (|_Take-In20197| |actions| |_Take-In20197|)
              (|_Take-In20197| |preparatory-event| |_Move20255|)
              (|_Take-In20197| |preparatory-event| |_Move20254|)
              (|_Take-In20197| |preparatory-event| |_Admit20253|)
              (|_Take-In20197| |primitive-actions| |_Take-In20197|)
              (|_Take-In20197| |time-during| |_Time-Interval20252|)
              (|_Take-In20197| |destination| |_Spatial-Entity20221|)
              (|_Take-In20197| |origin| |_Spatial-Entity20220|)
              (|_Take-In20197| |path| |_Spatial-Entity20222|)
              (|_Take-In20197| |base| |_Tangible-Entity20218|)
              (|_Take-In20197| |object| |_Tangible-Entity20236|)
              (|_Take-In20197| |resulting-state| |_Be-Contained20251|)
              (|_Take-In20197| |agent| |_Tangible-Entity20218|)
              (|_Take-In20197| |acceleration-magnitude|
               |_Acceleration-Magnitude-Value20227|)
              (|_Take-In20197| |acceleration|
               |_Acceleration-Vector-Value20226|)
              (|_Take-In20197| |distance| |_Length-Value20233|)
              (|_Take-In20197| |duration| |_Duration-Value20225|)
              (|_Take-In20197| |speed| |_Speed-Value20229|)
              (|_Take-In20197| |displacement|
               |_Displacement-Vector-Value20232|)
              (|_Take-In20197| |final-speed| |_Speed-Value20250|)
              (|_Take-In20197| |final-velocity|
               |_Velocity-Vector-Value20247|)
              (|_Take-In20197| |final-x-speed| |_Speed-Value20249|)
              (|_Take-In20197| |final-y-speed| |_Speed-Value20248|)
              (|_Take-In20197| |initial-speed| |_Speed-Value20246|)
              (|_Take-In20197| |initial-velocity|
               |_Velocity-Vector-Value20243|)
              (|_Take-In20197| |initial-x-speed| |_Speed-Value20245|)
              (|_Take-In20197| |initial-y-speed| |_Speed-Value20244|)
              (|_Take-In20197| |velocity|
               |_Velocity-Vector-Value20228|)
              (|_Take-In20197| |x-acceleration-magnitude|
               |_Acceleration-Magnitude-Value20242|)
              (|_Take-In20197| |x-distance| |_Length-Value20241|)
              (|_Take-In20197| |x-speed| |_Speed-Value20240|)
              (|_Take-In20197| |y-acceleration-magnitude|
               |_Acceleration-Magnitude-Value20239|)
              (|_Take-In20197| |y-distance| |_Length-Value20238|)
              (|_Take-In20197| |y-speed| |_Speed-Value20237|))
             T NIL)))
(|Arithmetic-Product| (|_Arithmetic-Product34970|
                       (((|_Arithmetic-Product34970| |instance-of|
                          |Arithmetic-Product|))
                        T NIL)))
(|Send| (|_Send2138|
         (((|_Move2278| |instance-of| |Move|)
           (|_Obtain2277| |instance-of| |Obtain|)
           (|_Time-Interval2276| |instance-of| |Time-Interval|)
           (|_Entity2273| |instance-of| |Tangible-Entity|)
           (|_Move2270| |instance-of| |Move|)
           (|_Obtain2269| |instance-of| |Obtain|)
           (|_Time-Interval2268| |instance-of| |Time-Interval|)
           (|_Move2263| |instance-of| |Move|)
           (|_Obtain2262| |instance-of| |Obtain|)
           (|_Time-Interval2261| |instance-of| |Time-Interval|)
           (|_Move2207| |instance-of| |Move|)
           (|_Obtain2206| |instance-of| |Obtain|)
           (|_Time-Interval2205| |instance-of| |Time-Interval|)
           (|_Entity2202| |instance-of| |Tangible-Entity|)
           (|_Move2199| |instance-of| |Move|)
           (|_Obtain2198| |instance-of| |Obtain|)
           (|_Time-Interval2197| |instance-of| |Time-Interval|)
           (|_Move2176| |instance-of| |Move|)
           (|_Obtain2175| |instance-of| |Obtain|)
           (|_Time-Interval2174| |instance-of| |Time-Interval|)
           (|_Move2152| |instance-of| |Move|)
           (|_Time-Interval2151| |instance-of| |Time-Interval|)
           (|_Tangible-Entity2147| |instance-of| |Tangible-Entity|)
           (|_Tangible-Entity2145| |instance-of| |Tangible-Entity|)
           (|_Deliver2150| |instance-of| |Deliver|)
           (|_Move2149| |instance-of| |Move|)
           (|_Relinquish2148| |instance-of| |Relinquish|)
           (|_Send2138| |instance-of| |Send|)
           (|_Entity2144| |instance-of| |Tangible-Entity|)
           (|_Obtain2269| |actions| |_Obtain2269|)
           (|_Obtain2269| |preparatory-event| |_Move2278|)
           (|_Obtain2269| |preparatory-event| |_Obtain2277|)
           (|_Obtain2269| |primitive-actions| |_Obtain2269|)
           (|_Obtain2269| |time-during| |_Time-Interval2276|)
           (|_Obtain2269| |object| |_Tangible-Entity2147|)
           (|_Obtain2269| |recipient| |_Entity2273|)
           (|_Obtain2269| |agent| |_Entity2273|)
           (|_Obtain2262| |actions| |_Obtain2262|)
           (|_Obtain2262| |preparatory-event| |_Move2270|)
           (|_Obtain2262| |preparatory-event| |_Obtain2269|)
           (|_Obtain2262| |primitive-actions| |_Obtain2262|)
           (|_Obtain2262| |time-during| |_Time-Interval2268|)
           (|_Obtain2262| |object| |_Tangible-Entity2147|)
           (|_Obtain2262| |recipient| |_Entity2144|)
           (|_Obtain2262| |agent| |_Entity2144|)
           (|_Deliver2150| |actions-of| |_Deliver2150|)
           (|_Deliver2150| |preparatory-event| |_Move2263|)
           (|_Deliver2150| |preparatory-event| |_Obtain2262|)
           (|_Deliver2150| |primitive-actions-of| |_Deliver2150|)
           (|_Deliver2150| |time-during| |_Time-Interval2261|)
           (|_Deliver2150| |object| |_Tangible-Entity2147|)
           (|_Deliver2150| |recipient| |_Tangible-Entity2145|)
           (|_Deliver2150| |donor| |_Entity2144|)
           (|_Obtain2198| |actions| |_Obtain2198|)
           (|_Obtain2198| |preparatory-event| |_Move2207|)
           (|_Obtain2198| |preparatory-event| |_Obtain2206|)
           (|_Obtain2198| |primitive-actions| |_Obtain2198|)
           (|_Obtain2198| |time-during| |_Time-Interval2205|)
           (|_Obtain2198| |object| |_Tangible-Entity2147|)
           (|_Obtain2198| |recipient| |_Entity2202|)
           (|_Obtain2198| |agent| |_Entity2202|)
           (|_Obtain2175| |actions| |_Obtain2175|)
           (|_Obtain2175| |preparatory-event| |_Move2199|)
           (|_Obtain2175| |preparatory-event| |_Obtain2198|)
           (|_Obtain2175| |primitive-actions| |_Obtain2175|)
           (|_Obtain2175| |time-during| |_Time-Interval2197|)
           (|_Obtain2175| |object| |_Tangible-Entity2147|)
           (|_Obtain2175| |recipient| |_Entity2144|)
           (|_Obtain2175| |agent| |_Entity2144|)
           (|_Relinquish2148| |actions-of| |_Relinquish2148|)
           (|_Relinquish2148| |preparatory-event| |_Move2176|)
           (|_Relinquish2148| |preparatory-event| |_Obtain2175|)
           (|_Relinquish2148| |primitive-actions-of| |_Relinquish2148|)
           (|_Relinquish2148| |time-during| |_Time-Interval2174|)
           (|_Relinquish2148| |object| |_Tangible-Entity2147|)
           (|_Relinquish2148| |next-event| |_Move2149|)
           (|_Relinquish2148| |agent| |_Entity2144|)
           (|_Relinquish2148| |donor| |_Entity2144|)
           (|_Send2138| |actions| |_Send2138|)
           (|_Send2138| |actions| |_Deliver2150|)
           (|_Send2138| |actions| |_Move2149|)
           (|_Send2138| |actions| |_Relinquish2148|)
           (|_Send2138| |all-subevents| |_Deliver2150|)
           (|_Send2138| |all-subevents| |_Move2149|)
           (|_Send2138| |all-subevents| |_Relinquish2148|)
           (|_Send2138| |preparatory-event| |_Move2152|)
           (|_Send2138| |primitive-actions| |_Deliver2150|)
           (|_Send2138| |primitive-actions| |_Move2149|)
           (|_Send2138| |primitive-actions| |_Relinquish2148|)
           (|_Send2138| |time-during| |_Time-Interval2151|)
           (|_Send2138| |destination| |_Tangible-Entity2145|)
           (|_Send2138| |origin| |_Entity2144|)
           (|_Send2138| |object| |_Tangible-Entity2147|)
           (|_Send2138| |recipient| |_Tangible-Entity2145|)
           (|_Send2138| |first-subevent| |_Relinquish2148|)
           (|_Send2138| |subevent| |_Deliver2150|)
           (|_Send2138| |subevent| |_Move2149|)
           (|_Send2138| |subevent| |_Relinquish2148|)
           (|_Send2138| |agent| |_Entity2144|)
           (|_Send2138| |donor| |_Entity2144|))
          T NIL)))
(|UoM-Pressure| (|_UoM-Pressure37444|
                 (((|_UoM-Pressure37444| |instance-of| |UoM-Pressure|))
                  T NIL)))
(|Divide| (|_Divide24920|
           (((|_Time-Interval24934| |instance-of| |Time-Interval|)
             (|_Physical-Object24930| |instance-of| |Physical-Object|)
             (|_Move24928| |instance-of| |Move|)
             (|_Time-Interval24927| |instance-of| |Time-Interval|)
             (|_Tangible-Entity24924| |instance-of| |Physical-Object|)
             (|_Divide24920| |instance-of| |Divide|)
             (|_Be-Broken24926| |instance-of| |Be-Broken|)
             (|_Be-Broken24926| |actions| |_Be-Broken24926|)
             (|_Be-Broken24926| |primitive-actions| |_Be-Broken24926|)
             (|_Be-Broken24926| |time-during| |_Time-Interval24934|)
             (|_Be-Broken24926| |object| |_Physical-Object24930|)
             (|_Divide24920| |actions| |_Divide24920|)
             (|_Divide24920| |preparatory-event| |_Move24928|)
             (|_Divide24920| |primitive-actions| |_Divide24920|)
             (|_Divide24920| |time-during| |_Time-Interval24927|)
             (|_Divide24920| |object| |_Tangible-Entity24924|)
             (|_Divide24920| |resulting-state| |_Be-Broken24926|))
            T NIL)))
(|Texture-Value| (|_Texture-Value37690|
                  (((|_Texture-Value37690| |instance-of|
                     |Texture-Value|))
                   T NIL)))
(|Signal| (|_Signal32944|
           (((|_Transmit32946| |instance-of| |Transmit|)
             (|_Signal32944| |instance-of| |Signal|)
             (|_Entity32945| |instance-of| |Entity|)
             (|_Signal32944| |in-event| |_Transmit32946|)
             (|_Signal32944| |played-by| |_Entity32945|))
            T NIL)))
(|Growth| (|_Growth839|
           (((|_Growth839| |instance-of| |Growth|)
             (|_Time-Interval845| |instance-of| |Time-Interval|)
             (|_Growth839| |actions| |_Growth839|)
             (|_Growth839| |primitive-actions| |_Growth839|)
             (|_Growth839| |time-during| |_Time-Interval845|))
            T NIL)))
(|Political-State| (|_Political-State34681|
                    (((|_Political-State34681| |instance-of|
                       |Political-State|))
                     T NIL)))
(|NL-Slot-Group| (|_NL-Slot-Group37965|
                  (((|_NL-Slot-Group37965| |instance-of|
                     |NL-Slot-Group|))
                   T NIL)))
(|Edit| (|_Edit25242|
         (((|_Move25249| |instance-of| |Move|)
           (|_Time-Interval25248| |instance-of| |Time-Interval|)
           (|_Edit25242| |instance-of| |Edit|)
           (|_Document25246| |instance-of| |Document|)
           (|_Document25246| |instance-of| |Spatial-Entity|)
           (|_Edit25242| |actions| |_Edit25242|)
           (|_Edit25242| |preparatory-event| |_Move25249|)
           (|_Edit25242| |primitive-actions| |_Edit25242|)
           (|_Edit25242| |time-during| |_Time-Interval25248|)
           (|_Edit25242| |object| |_Document25246|))
          T NIL)))
(|Department| (|_Department34877|
               (((|_Department34877| |instance-of| |Department|)
                 (|_Number34879| |instance-of| |Number|)
                 (|_Department34877| |number-of-elements|
                  |_Number34879|))
                T NIL)))
(|Diameter-Scale| (|_Diameter-Scale37612|
                   (((|_Diameter-Scale37612| |instance-of|
                      |Diameter-Scale|)
                     (|_Number37614| |instance-of| |Number|)
                     (|_Diameter-Scale37612| |number-of-elements|
                      |_Number37614|))
                    T NIL)))
(|Replace| (|_Replace9884|
            (((|_Text-Field9928| |instance-of| |Text-Field|)
              (|_Move9927| |instance-of| |Move|)
              (|_Make-Contact9926| |instance-of| |Make-Contact|)
              (|_Time-Interval9925| |instance-of| |Time-Interval|)
              (|_Be-Attached-To9924| |instance-of| |Be-Attached-To|)
              (|_Text-Field9923| |instance-of| |Text-Field|)
              (|_Move9922| |instance-of| |Move|)
              (|_Attach9921| |instance-of| |Attach|)
              (|_Time-Interval9920| |instance-of| |Time-Interval|)
              (|_Move9917| |instance-of| |Move|)
              (|_Time-Interval9916| |instance-of| |Time-Interval|)
              (|_Tangible-Entity9915| |instance-of| |Tangible-Entity|)
              (|_Acceleration-Magnitude-Value9914| |instance-of|
               |Acceleration-Magnitude-Value|)
              (|_Acceleration-Vector-Value9898| |instance-of|
               |Acceleration-Vector-Value|)
              (|_Length-Value9913| |instance-of| |Length-Value|)
              (|_Duration-Value9912| |instance-of| |Duration-Value|)
              (|_Speed-Value9911| |instance-of| |Speed-Value|)
              (|_Displacement-Vector-Value9896| |instance-of|
               |Displacement-Vector-Value|)
              (|_Speed-Value9910| |instance-of| |Speed-Value|)
              (|_Velocity-Vector-Value9907| |instance-of|
               |Velocity-Vector-Value|)
              (|_Speed-Value9909| |instance-of| |Speed-Value|)
              (|_Speed-Value9908| |instance-of| |Speed-Value|)
              (|_Speed-Value9906| |instance-of| |Speed-Value|)
              (|_Velocity-Vector-Value9903| |instance-of|
               |Velocity-Vector-Value|)
              (|_Speed-Value9905| |instance-of| |Speed-Value|)
              (|_Speed-Value9904| |instance-of| |Speed-Value|)
              (|_Velocity-Vector-Value9894| |instance-of|
               |Velocity-Vector-Value|)
              (|_Acceleration-Magnitude-Value9902| |instance-of|
               |Acceleration-Magnitude-Value|)
              (|_Length-Value9901| |instance-of| |Length-Value|)
              (|_Speed-Value9900| |instance-of| |Speed-Value|)
              (|_Acceleration-Magnitude-Value9899| |instance-of|
               |Acceleration-Magnitude-Value|)
              (|_Length-Value9897| |instance-of| |Length-Value|)
              (|_Speed-Value9895| |instance-of| |Speed-Value|)
              (|_Text-Field9893| |instance-of| |Text-Field|)
              (|_Text-Field9892| |instance-of| |Text-Field|)
              (|_Move9891| |instance-of| |Move|)
              (|_Time-Interval9890| |instance-of| |Time-Interval|)
              (|_Tangible-Entity9889| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity9888| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity9887| |instance-of| |Tangible-Entity|)
              (|_Attach9886| |instance-of| |Attach|)
              (|_Replace9884| |instance-of| |Replace|)
              (|_Detach9885| |instance-of| |Detach|)
              (|_Attach9886| |actions-of| |_Attach9886|)
              (|_Attach9886| |identifier| |_Text-Field9928|)
              (|_Attach9886| |preparatory-event| |_Move9927|)
              (|_Attach9886| |preparatory-event| |_Make-Contact9926|)
              (|_Attach9886| |primitive-actions-of| |_Attach9886|)
              (|_Attach9886| |time-during| |_Time-Interval9925|)
              (|_Attach9886| |base| |_Tangible-Entity9889|)
              (|_Attach9886| |object| |_Tangible-Entity9887|)
              (|_Attach9886| |resulting-state| |_Be-Attached-To9924|)
              (|_Detach9885| |actions-of| |_Detach9885|)
              (|_Detach9885| |identifier| |_Text-Field9923|)
              (|_Detach9885| |preparatory-event| |_Move9922|)
              (|_Detach9885| |preparatory-event| |_Attach9921|)
              (|_Detach9885| |primitive-actions-of| |_Detach9885|)
              (|_Detach9885| |time-during| |_Time-Interval9920|)
              (|_Detach9885| |base| |_Tangible-Entity9889|)
              (|_Detach9885| |object| |_Tangible-Entity9887|)
              (|_Detach9885| |next-event| |_Attach9886|)
              (|_Move9891| |actions| |_Move9891|)
              (|_Move9891| |preparatory-event| |_Move9917|)
              (|_Move9891| |primitive-actions| |_Move9891|)
              (|_Move9891| |time-during| |_Time-Interval9916|)
              (|_Move9891| |destination| |_Tangible-Entity9887|)
              (|_Move9891| |object| |_Tangible-Entity9915|)
              (|_Move9891| |acceleration-magnitude|
               |_Acceleration-Magnitude-Value9914|)
              (|_Move9891| |acceleration|
               |_Acceleration-Vector-Value9898|)
              (|_Move9891| |distance| |_Length-Value9913|)
              (|_Move9891| |duration| |_Duration-Value9912|)
              (|_Move9891| |speed| |_Speed-Value9911|)
              (|_Move9891| |displacement|
               |_Displacement-Vector-Value9896|)
              (|_Move9891| |final-speed| |_Speed-Value9910|)
              (|_Move9891| |final-velocity|
               |_Velocity-Vector-Value9907|)
              (|_Move9891| |final-x-speed| |_Speed-Value9909|)
              (|_Move9891| |final-y-speed| |_Speed-Value9908|)
              (|_Move9891| |initial-speed| |_Speed-Value9906|)
              (|_Move9891| |initial-velocity|
               |_Velocity-Vector-Value9903|)
              (|_Move9891| |initial-x-speed| |_Speed-Value9905|)
              (|_Move9891| |initial-y-speed| |_Speed-Value9904|)
              (|_Move9891| |velocity| |_Velocity-Vector-Value9894|)
              (|_Move9891| |x-acceleration-magnitude|
               |_Acceleration-Magnitude-Value9902|)
              (|_Move9891| |x-distance| |_Length-Value9901|)
              (|_Move9891| |x-speed| |_Speed-Value9900|)
              (|_Move9891| |y-acceleration-magnitude|
               |_Acceleration-Magnitude-Value9899|)
              (|_Move9891| |y-distance| |_Length-Value9897|)
              (|_Move9891| |y-speed| |_Speed-Value9895|)
              (|_Tangible-Entity9888| |identifier| |_Text-Field9893|)
              (|_Tangible-Entity9887| |identifier| |_Text-Field9892|)
              (|_Replace9884| |actions| |_Replace9884|)
              (|_Replace9884| |actions| |_Attach9886|)
              (|_Replace9884| |actions| |_Detach9885|)
              (|_Replace9884| |all-subevents| |_Attach9886|)
              (|_Replace9884| |all-subevents| |_Detach9885|)
              (|_Replace9884| |preparatory-event| |_Move9891|)
              (|_Replace9884| |primitive-actions| |_Attach9886|)
              (|_Replace9884| |primitive-actions| |_Detach9885|)
              (|_Replace9884| |time-during| |_Time-Interval9890|)
              (|_Replace9884| |base| |_Tangible-Entity9889|)
              (|_Replace9884| |object| |_Tangible-Entity9888|)
              (|_Replace9884| |object| |_Tangible-Entity9887|)
              (|_Replace9884| |first-subevent| |_Detach9885|)
              (|_Replace9884| |subevent| |_Attach9886|)
              (|_Replace9884| |subevent| |_Detach9885|))
             NIL NIL)))
(|Length-Constant| (|_Length-Constant37873|
                    (((|_Length-Constant37873| |instance-of|
                       |Length-Constant|))
                     T NIL)))
(|Physical-Mark| (|_Physical-Mark34136|
                  (((|_Mark34138| |instance-of| |Mark|)
                    (|_Physical-Mark34136| |instance-of|
                     |Physical-Mark|)
                    (|_Substance34137| |instance-of| |Substance|)
                    (|_Physical-Mark34136| |result-of| |_Mark34138|)
                    (|_Physical-Mark34136| |material|
                     |_Substance34137|))
                   T NIL)))
(|Charge-Value| (|_Charge-Value37681|
                 (((|_Charge-Value37681| |instance-of| |Charge-Value|))
                  T NIL)))
(|Inquire| (|_Inquire25185|
            (((|_Move25198| |instance-of| |Move|)
              (|_Time-Interval25197| |instance-of| |Time-Interval|)
              (|_Information25195| |instance-of| |Information|)
              (|_Information25195| |instance-of| |Spatial-Entity|)
              (|_Tangible-Entity25193| |instance-of| |Tangible-Entity|)
              (|_Message25196| |instance-of| |Message|)
              (|_Inquire25185| |instance-of| |Inquire|)
              (|_Tangible-Entity25192| |instance-of| |Tangible-Entity|)
              (|_Inquire25185| |actions| |_Inquire25185|)
              (|_Inquire25185| |preparatory-event| |_Move25198|)
              (|_Inquire25185| |primitive-actions| |_Inquire25185|)
              (|_Inquire25185| |time-during| |_Time-Interval25197|)
              (|_Inquire25185| |object| |_Information25195|)
              (|_Inquire25185| |recipient| |_Tangible-Entity25193|)
              (|_Inquire25185| |result| |_Message25196|)
              (|_Inquire25185| |agent| |_Tangible-Entity25192|))
             T NIL)))
(|Teacher| (|_Teacher32940|
            (((|_Teacher32940| |instance-of| |Teacher|)
              (|_Tangible-Entity32941| |instance-of| |Tangible-Entity|)
              (|_Teacher32940| |played-by| |_Tangible-Entity32941|))
             T NIL)))
(|Duplicate| (|_Duplicate25814|
              (((|_Move25823| |instance-of| |Move|)
                (|_Time-Interval25822| |instance-of| |Time-Interval|)
                (|_Tangible-Entity25819| |instance-of|
                 |Tangible-Entity|)
                (|_Duplicate25814| |instance-of| |Duplicate|)
                (|_Entity25821| |instance-of| |Entity|)
                (|_Duplicate25814| |actions| |_Duplicate25814|)
                (|_Duplicate25814| |preparatory-event| |_Move25823|)
                (|_Duplicate25814| |primitive-actions|
                 |_Duplicate25814|)
                (|_Duplicate25814| |time-during| |_Time-Interval25822|)
                (|_Duplicate25814| |object| |_Tangible-Entity25819|)
                (|_Duplicate25814| |result| |_Entity25821|))
               T NIL)))
(|Express| (|_Express25041|
            (((|_Move25052| |instance-of| |Move|)
              (|_Time-Interval25051| |instance-of| |Time-Interval|)
              (|_Information25049| |instance-of| |Information|)
              (|_Information25049| |instance-of| |Spatial-Entity|)
              (|_Message25050| |instance-of| |Message|)
              (|_Express25041| |instance-of| |Express|)
              (|_Tangible-Entity25047| |instance-of| |Tangible-Entity|)
              (|_Express25041| |actions| |_Express25041|)
              (|_Express25041| |preparatory-event| |_Move25052|)
              (|_Express25041| |primitive-actions| |_Express25041|)
              (|_Express25041| |time-during| |_Time-Interval25051|)
              (|_Express25041| |object| |_Information25049|)
              (|_Express25041| |result| |_Message25050|)
              (|_Express25041| |agent| |_Tangible-Entity25047|))
             T NIL)))
(|Decrease| (|_Decrease25372|
             (((|_Time-Interval25380| |instance-of| |Time-Interval|)
               (|_Property-Value25378| |instance-of| |Property-Value|)
               (|_Decrease25372| |instance-of| |Decrease|)
               (|_Property-Value25379| |instance-of| |Property-Value|)
               (|_Decrease25372| |actions| |_Decrease25372|)
               (|_Decrease25372| |primitive-actions| |_Decrease25372|)
               (|_Decrease25372| |time-during| |_Time-Interval25380|)
               (|_Decrease25372| |from-value| |_Property-Value25378|)
               (|_Decrease25372| |to-value| |_Property-Value25379|))
              T NIL)))
(|UoM-Volume| (|_UoM-Volume37426|
               (((|_UoM-Volume37426| |instance-of| |UoM-Volume|)) T
                NIL)))
(|Capacity-Value| (|_Capacity-Value37751|
                   (((|_Capacity-Value37751| |instance-of|
                      |Capacity-Value|))
                    T NIL)))
(|Confine| (|_Confine23838|
            (((|_Container23860| |instance-of| |Container|)
              (|_Time-Interval23857| |instance-of| |Time-Interval|)
              (|_Tangible-Entity23851| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity23853| |instance-of| |Tangible-Entity|)
              (|_Move23849| |instance-of| |Move|)
              (|_Admit23847| |instance-of| |Admit|)
              (|_Time-Interval23846| |instance-of| |Time-Interval|)
              (|_Tangible-Entity23845| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity23842| |instance-of| |Tangible-Entity|)
              (|_Confine23838| |instance-of| |Confine|)
              (|_Be-Confined23844| |instance-of| |Be-Confined|)
              (|_Tangible-Entity23845| |plays| |_Container23860|)
              (|_Tangible-Entity23853| |is-inside|
               |_Tangible-Entity23851|)
              (|_Be-Confined23844| |actions| |_Be-Confined23844|)
              (|_Be-Confined23844| |primitive-actions|
               |_Be-Confined23844|)
              (|_Be-Confined23844| |time-during| |_Time-Interval23857|)
              (|_Be-Confined23844| |base| |_Tangible-Entity23851|)
              (|_Be-Confined23844| |object| |_Tangible-Entity23853|)
              (|_Confine23838| |actions| |_Confine23838|)
              (|_Confine23838| |preparatory-event| |_Move23849|)
              (|_Confine23838| |preparatory-event| |_Admit23847|)
              (|_Confine23838| |primitive-actions| |_Confine23838|)
              (|_Confine23838| |time-during| |_Time-Interval23846|)
              (|_Confine23838| |base| |_Tangible-Entity23845|)
              (|_Confine23838| |object| |_Tangible-Entity23842|)
              (|_Confine23838| |resulting-state| |_Be-Confined23844|))
             T NIL)))
(|Boat| (|_Boat34603| (((|_Boat34603| |instance-of| |Boat|)) T NIL)))
(|Time-Interval| (|_Time-Interval34894|
                  (((|_Time-Interval34894| |instance-of|
                     |Time-Interval|))
                   T NIL)))
(|Territory| (|_Territory34675|
              (((|_Territory34675| |instance-of| |Territory|)) T NIL)))
(|Be-Unblocked| (|_Be-Unblocked386|
                 (((|_Time-Interval390| |instance-of| |Time-Interval|)
                   (|_Be-Unblocked386| |instance-of| |Be-Unblocked|)
                   (|_Spatial-Entity389| |instance-of|
                    |Spatial-Entity|)
                   (|_Be-Unblocked386| |actions| |_Be-Unblocked386|)
                   (|_Be-Unblocked386| |primitive-actions|
                    |_Be-Unblocked386|)
                   (|_Be-Unblocked386| |time-during|
                    |_Time-Interval390|)
                   (|_Be-Unblocked386| |object| |_Spatial-Entity389|))
                  T NIL)))
(|Intensity-Value| (|_Intensity-Value37739|
                    (((|_Intensity-Value37739| |instance-of|
                       |Intensity-Value|))
                     T NIL)))
(|SHAKEN-Partition| (|_SHAKEN-Partition37977|
                     (((|_SHAKEN-Partition37977| |instance-of|
                        |SHAKEN-Partition|))
                      T NIL)))
(|Discussion| (|_Discussion794|
               (((|_Discussion794| |instance-of| |Discussion|)
                 (|_Time-Interval800| |instance-of| |Time-Interval|)
                 (|_Discussion794| |actions| |_Discussion794|)
                 (|_Discussion794| |primitive-actions|
                  |_Discussion794|)
                 (|_Discussion794| |time-during| |_Time-Interval800|))
                T NIL)))
(|Rate-Constant| (|_Rate-Constant37841|
                  (((|_Rate-Constant37841| |instance-of|
                     |Rate-Constant|))
                   T NIL)))
(|UoM-Quantity| (|_UoM-Quantity37442|
                 (((|_UoM-Quantity37442| |instance-of| |UoM-Quantity|))
                  T NIL)))
(|Piece-of-Substance| (|_Piece-of-Substance34037|
                       (((|_Piece-of-Substance34037| |instance-of|
                          |Piece-of-Substance|)
                         (|_Substance34040| |instance-of| |Substance|)
                         (|_Piece-of-Substance34037| |material|
                          |_Substance34040|))
                        T NIL)))
(|Taste-Constant| (|_Taste-Constant37823|
                   (((|_Taste-Constant37823| |instance-of|
                      |Taste-Constant|))
                    T NIL)))
(|Rotational-Rate-Value| (|_Rotational-Rate-Value37704|
                          (((|_Rotational-Rate-Value37704|
                             |instance-of| |Rotational-Rate-Value|))
                           T NIL)))
(|Learn| (|_Learn24618|
          (((|_Time-Interval24635| |instance-of| |Time-Interval|)
            (|_Tangible-Entity24634| |instance-of| |Living-Entity|)
            (|_Entity24630| |instance-of| |Entity|)
            (|_Move24628| |instance-of| |Move|)
            (|_Time-Interval24627| |instance-of| |Time-Interval|)
            (|_Message24625| |instance-of| |Message|)
            (|_Message24625| |instance-of| |Spatial-Entity|)
            (|_Be-Known24626| |instance-of| |Be-Known|)
            (|_Learn24618| |instance-of| |Learn|)
            (|_Tangible-Entity24623| |instance-of| |Tangible-Entity|)
            (|_Be-Known24626| |actions| |_Be-Known24626|)
            (|_Be-Known24626| |primitive-actions| |_Be-Known24626|)
            (|_Be-Known24626| |time-during| |_Time-Interval24635|)
            (|_Be-Known24626| |experiencer| |_Tangible-Entity24634|)
            (|_Be-Known24626| |object| |_Entity24630|)
            (|_Learn24618| |actions| |_Learn24618|)
            (|_Learn24618| |preparatory-event| |_Move24628|)
            (|_Learn24618| |primitive-actions| |_Learn24618|)
            (|_Learn24618| |time-during| |_Time-Interval24627|)
            (|_Learn24618| |object| |_Message24625|)
            (|_Learn24618| |resulting-state| |_Be-Known24626|)
            (|_Learn24618| |agent| |_Tangible-Entity24623|))
           T NIL)))
(|Reporting| (|_Reporting480|
              (((|_Reporting480| |instance-of| |Reporting|)
                (|_Time-Interval486| |instance-of| |Time-Interval|)
                (|_Reporting480| |actions| |_Reporting480|)
                (|_Reporting480| |primitive-actions| |_Reporting480|)
                (|_Reporting480| |time-during| |_Time-Interval486|))
               T NIL)))
(|Educational-Institution| (|_Educational-Institution34832|
                            (((|_Educational-Institution34832|
                               |instance-of| |Educational-Institution|)
                              (|_Number34834| |instance-of| |Number|)
                              (|_Educational-Institution34832|
                               |number-of-elements| |_Number34834|))
                             T NIL)))
(|See| (|_See25482|
        (((|_Move25492| |instance-of| |Move|)
          (|_Time-Interval25491| |instance-of| |Time-Interval|)
          (|_Tangible-Entity25490| |instance-of| |Tangible-Entity|)
          (|_Light25487| |instance-of| |Light|)
          (|_See25482| |instance-of| |See|)
          (|_Message25489| |instance-of| |Message|)
          (|_See25482| |actions| |_See25482|)
          (|_See25482| |preparatory-event| |_Move25492|)
          (|_See25482| |primitive-actions| |_See25482|)
          (|_See25482| |time-during| |_Time-Interval25491|)
          (|_See25482| |experiencer| |_Tangible-Entity25490|)
          (|_See25482| |object| |_Light25487|)
          (|_See25482| |result| |_Message25489|))
         T NIL)))
(|Stand-Up| (|_Stand-Up10271|
             (((|_Time-Interval10284| |instance-of| |Time-Interval|)
               (|_Entity10279| |instance-of| |Entity|)
               (|_Time-Interval10278| |instance-of| |Time-Interval|)
               (|_Be-Standing10277| |instance-of| |Be-Standing|)
               (|_Stand-Up10271| |instance-of| |Stand-Up|)
               (|_Entity10275| |instance-of| |Tangible-Entity|)
               (|_Be-Standing10277| |actions| |_Be-Standing10277|)
               (|_Be-Standing10277| |primitive-actions|
                |_Be-Standing10277|)
               (|_Be-Standing10277| |time-during|
                |_Time-Interval10284|)
               (|_Be-Standing10277| |object| |_Entity10279|)
               (|_Stand-Up10271| |actions| |_Stand-Up10271|)
               (|_Stand-Up10271| |primitive-actions| |_Stand-Up10271|)
               (|_Stand-Up10271| |time-during| |_Time-Interval10278|)
               (|_Stand-Up10271| |object| |_Entity10275|)
               (|_Stand-Up10271| |resulting-state| |_Be-Standing10277|)
               (|_Stand-Up10271| |agent| |_Entity10275|))
              T NIL)))
(|Vertex| (|_Vertex34707|
           (((|_Line34711| |instance-of| |Line|)
             (|_Line34710| |instance-of| |Line|)
             (|_Vertex34707| |instance-of| |Vertex|)
             (|_Angle-Value34709| |instance-of| |Angle-Value|)
             (|_Vertex34707| |is-part-of| |_Line34711|)
             (|_Vertex34707| |is-part-of| |_Line34710|)
             (|_Vertex34707| |angle| |_Angle-Value34709|))
            T NIL)))
(|Triangle| (|_Triangle34720|
             (((|_Triangle34720| |instance-of| |Triangle|)) T NIL)))
(|Temporal-Entity| (|_Temporal-Entity34892|
                    (((|_Temporal-Entity34892| |instance-of|
                       |Temporal-Entity|))
                     T NIL)))
(|Release-Resource| (|_Release-Resource9982|
                     (((|_Time-Interval9997| |instance-of|
                        |Time-Interval|)
                       (|_Resource9993| |instance-of| |Resource|)
                       (|_Move9991| |instance-of| |Move|)
                       (|_Allocate-Resource9990| |instance-of|
                        |Allocate-Resource|)
                       (|_Time-Interval9989| |instance-of|
                        |Time-Interval|)
                       (|_Resource9986| |instance-of| |Resource|)
                       (|_Resource9986| |instance-of| |Spatial-Entity|)
                       (|_Release-Resource9982| |instance-of|
                        |Release-Resource|)
                       (|_Be-Available9988| |instance-of|
                        |Be-Available|)
                       (|_Be-Available9988| |actions|
                        |_Be-Available9988|)
                       (|_Be-Available9988| |primitive-actions|
                        |_Be-Available9988|)
                       (|_Be-Available9988| |time-during|
                        |_Time-Interval9997|)
                       (|_Be-Available9988| |object| |_Resource9993|)
                       (|_Release-Resource9982| |actions|
                        |_Release-Resource9982|)
                       (|_Release-Resource9982| |preparatory-event|
                        |_Move9991|)
                       (|_Release-Resource9982| |preparatory-event|
                        |_Allocate-Resource9990|)
                       (|_Release-Resource9982| |primitive-actions|
                        |_Release-Resource9982|)
                       (|_Release-Resource9982| |time-during|
                        |_Time-Interval9989|)
                       (|_Release-Resource9982| |object|
                        |_Resource9986|)
                       (|_Release-Resource9982| |resulting-state|
                        |_Be-Available9988|))
                      T NIL)))
(|Carry| (|_Carry23270|
          (((|_Move23404| |instance-of| |Move|)
            (|_Make-Inaccessible23403| |instance-of|
             |Make-Inaccessible|)
            (|_Time-Interval23402| |instance-of| |Time-Interval|)
            (|_Time-Interval23395| |instance-of| |Time-Interval|)
            (|_Tangible-Entity23391| |instance-of| |Tangible-Entity|)
            (|_Tangible-Entity23389| |instance-of| |Tangible-Entity|)
            (|_Move23388| |instance-of| |Move|)
            (|_Make-Accessible23387| |instance-of| |Make-Accessible|)
            (|_Time-Interval23386| |instance-of| |Time-Interval|)
            (|_Be-Held23385| |instance-of| |Be-Held|)
            (|_Tangible-Entity23357| |instance-of| |Tangible-Entity|)
            (|_Move23315| |instance-of| |Move|)
            (|_Time-Interval23314| |instance-of| |Time-Interval|)
            (|_Tangible-Entity23297| |instance-of| |Tangible-Entity|)
            (|_Hold23313| |instance-of| |Hold|)
            (|_Locomotion23312| |instance-of| |Locomotion|)
            (|_Tangible-Entity23285| |instance-of| |Tangible-Entity|)
            (|_Acceleration-Magnitude-Value23288| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value23287| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value23294| |instance-of| |Length-Value|)
            (|_Duration-Value23286| |instance-of| |Duration-Value|)
            (|_Speed-Value23290| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value23293| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value23311| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value23308| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value23310| |instance-of| |Speed-Value|)
            (|_Speed-Value23309| |instance-of| |Speed-Value|)
            (|_Speed-Value23307| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value23304| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value23306| |instance-of| |Speed-Value|)
            (|_Speed-Value23305| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value23289| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value23303| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value23302| |instance-of| |Length-Value|)
            (|_Speed-Value23301| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value23300| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value23299| |instance-of| |Length-Value|)
            (|_Carry23270| |instance-of| |Carry|)
            (|_Speed-Value23298| |instance-of| |Speed-Value|)
            (|_Make-Accessible23387| |actions| |_Make-Accessible23387|)
            (|_Make-Accessible23387| |preparatory-event| |_Move23404|)
            (|_Make-Accessible23387| |preparatory-event|
             |_Make-Inaccessible23403|)
            (|_Make-Accessible23387| |primitive-actions|
             |_Make-Accessible23387|)
            (|_Make-Accessible23387| |time-during|
             |_Time-Interval23402|)
            (|_Make-Accessible23387| |object| |_Tangible-Entity23297|)
            (|_Make-Accessible23387| |agent| |_Tangible-Entity23357|)
            (|_Be-Held23385| |actions| |_Be-Held23385|)
            (|_Be-Held23385| |primitive-actions| |_Be-Held23385|)
            (|_Be-Held23385| |time-during| |_Time-Interval23395|)
            (|_Be-Held23385| |object| |_Tangible-Entity23391|)
            (|_Be-Held23385| |agent| |_Tangible-Entity23389|)
            (|_Hold23313| |actions-of| |_Hold23313|)
            (|_Hold23313| |preparatory-event| |_Move23388|)
            (|_Hold23313| |preparatory-event| |_Make-Accessible23387|)
            (|_Hold23313| |primitive-actions-of| |_Hold23313|)
            (|_Hold23313| |time-during| |_Time-Interval23386|)
            (|_Hold23313| |object| |_Tangible-Entity23297|)
            (|_Hold23313| |resulting-state| |_Be-Held23385|)
            (|_Hold23313| |agent| |_Tangible-Entity23357|)
            (|_Carry23270| |actions| |_Carry23270|)
            (|_Carry23270| |actions| |_Hold23313|)
            (|_Carry23270| |actions| |_Locomotion23312|)
            (|_Carry23270| |all-subevents| |_Hold23313|)
            (|_Carry23270| |all-subevents| |_Locomotion23312|)
            (|_Carry23270| |preparatory-event| |_Move23315|)
            (|_Carry23270| |primitive-actions| |_Hold23313|)
            (|_Carry23270| |primitive-actions| |_Locomotion23312|)
            (|_Carry23270| |time-during| |_Time-Interval23314|)
            (|_Carry23270| |object| |_Tangible-Entity23297|)
            (|_Carry23270| |first-subevent| |_Hold23313|)
            (|_Carry23270| |first-subevent| |_Locomotion23312|)
            (|_Carry23270| |subevent| |_Hold23313|)
            (|_Carry23270| |subevent| |_Locomotion23312|)
            (|_Carry23270| |agent| |_Tangible-Entity23285|)
            (|_Carry23270| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value23288|)
            (|_Carry23270| |acceleration|
             |_Acceleration-Vector-Value23287|)
            (|_Carry23270| |distance| |_Length-Value23294|)
            (|_Carry23270| |duration| |_Duration-Value23286|)
            (|_Carry23270| |speed| |_Speed-Value23290|)
            (|_Carry23270| |displacement|
             |_Displacement-Vector-Value23293|)
            (|_Carry23270| |final-speed| |_Speed-Value23311|)
            (|_Carry23270| |final-velocity|
             |_Velocity-Vector-Value23308|)
            (|_Carry23270| |final-x-speed| |_Speed-Value23310|)
            (|_Carry23270| |final-y-speed| |_Speed-Value23309|)
            (|_Carry23270| |initial-speed| |_Speed-Value23307|)
            (|_Carry23270| |initial-velocity|
             |_Velocity-Vector-Value23304|)
            (|_Carry23270| |initial-x-speed| |_Speed-Value23306|)
            (|_Carry23270| |initial-y-speed| |_Speed-Value23305|)
            (|_Carry23270| |velocity| |_Velocity-Vector-Value23289|)
            (|_Carry23270| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value23303|)
            (|_Carry23270| |x-distance| |_Length-Value23302|)
            (|_Carry23270| |x-speed| |_Speed-Value23301|)
            (|_Carry23270| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value23300|)
            (|_Carry23270| |y-distance| |_Length-Value23299|)
            (|_Carry23270| |y-speed| |_Speed-Value23298|))
           T NIL)))
(|Donor-Role| (|_Donor-Role33031|
               (((|_Donor-Role33031| |instance-of| |Donor-Role|)
                 (|_Entity33033| |instance-of| |Entity|)
                 (|_Donor-Role33031| |played-by| |_Entity33033|))
                T NIL)))
(|Group| (|_Group34852|
          (((|_Group34852| |instance-of| |Group|)
            (|_Number34854| |instance-of| |Number|)
            (|_Group34852| |number-of-elements| |_Number34854|))
           T NIL)))
(|Car| (|_Car34609|
        (((|_Car34609| |instance-of| |Car|)
          (|_Vehicle34611| |instance-of| |Vehicle|)
          (|_Car34609| |purpose| |_Vehicle34611|))
         T NIL)))
(|Constant| (|_Constant37795|
             (((|_Constant37795| |instance-of| |Constant|)) T NIL)))
(|Hold| (|_Hold23786|
         (((|_Move23809| |instance-of| |Move|)
           (|_Make-Inaccessible23808| |instance-of|
            |Make-Inaccessible|)
           (|_Time-Interval23807| |instance-of| |Time-Interval|)
           (|_Time-Interval23800| |instance-of| |Time-Interval|)
           (|_Tangible-Entity23796| |instance-of| |Tangible-Entity|)
           (|_Tangible-Entity23794| |instance-of| |Tangible-Entity|)
           (|_Move23793| |instance-of| |Move|)
           (|_Make-Accessible23792| |instance-of| |Make-Accessible|)
           (|_Time-Interval23791| |instance-of| |Time-Interval|)
           (|_Tangible-Entity23789| |instance-of| |Tangible-Entity|)
           (|_Be-Held23790| |instance-of| |Be-Held|)
           (|_Hold23786| |instance-of| |Hold|)
           (|_Tangible-Entity23787| |instance-of| |Tangible-Entity|)
           (|_Make-Accessible23792| |actions| |_Make-Accessible23792|)
           (|_Make-Accessible23792| |preparatory-event| |_Move23809|)
           (|_Make-Accessible23792| |preparatory-event|
            |_Make-Inaccessible23808|)
           (|_Make-Accessible23792| |primitive-actions|
            |_Make-Accessible23792|)
           (|_Make-Accessible23792| |time-during|
            |_Time-Interval23807|)
           (|_Make-Accessible23792| |object| |_Tangible-Entity23789|)
           (|_Make-Accessible23792| |agent| |_Tangible-Entity23787|)
           (|_Be-Held23790| |actions| |_Be-Held23790|)
           (|_Be-Held23790| |primitive-actions| |_Be-Held23790|)
           (|_Be-Held23790| |time-during| |_Time-Interval23800|)
           (|_Be-Held23790| |object| |_Tangible-Entity23796|)
           (|_Be-Held23790| |agent| |_Tangible-Entity23794|)
           (|_Hold23786| |actions| |_Hold23786|)
           (|_Hold23786| |preparatory-event| |_Move23793|)
           (|_Hold23786| |preparatory-event| |_Make-Accessible23792|)
           (|_Hold23786| |primitive-actions| |_Hold23786|)
           (|_Hold23786| |time-during| |_Time-Interval23791|)
           (|_Hold23786| |object| |_Tangible-Entity23789|)
           (|_Hold23786| |resulting-state| |_Be-Held23790|)
           (|_Hold23786| |agent| |_Tangible-Entity23787|))
          T NIL)))
(|University| (|_University34837|
               (((|_University34837| |instance-of| |University|)
                 (|_Number34839| |instance-of| |Number|)
                 (|_University34837| |number-of-elements|
                  |_Number34839|))
                T NIL)))
(|Furniture| (|_Furniture34170|
              (((|_Time-Interval34175| |instance-of| |Time-Interval|)
                (|_Furniture34170| |instance-of| |Furniture|)
                (|_Create34172| |instance-of| |Create|)
                (|_Create34172| |actions| |_Create34172|)
                (|_Create34172| |primitive-actions| |_Create34172|)
                (|_Create34172| |time-during| |_Time-Interval34175|)
                (|_Furniture34170| |result-of| |_Create34172|))
               T NIL)))
(|Helicopter| (|_Helicopter34614|
               (((|_Helicopter34614| |instance-of| |Helicopter|)
                 (|_Vehicle34616| |instance-of| |Vehicle|)
                 (|_Helicopter34614| |purpose| |_Vehicle34616|))
                T NIL)))
(|Rope| (|_Rope34629| (((|_Rope34629| |instance-of| |Rope|)) T NIL)))
(|Substance| (|_Substance33605|
              (((|_Substance33605| |instance-of| |Substance|)) T NIL)))
(|Unblock| (|_Unblock24461|
            (((|_Move24467| |instance-of| |Move|)
              (|_Block24465| |instance-of| |Block|)
              (|_Time-Interval24464| |instance-of| |Time-Interval|)
              (|_Unblock24461| |instance-of| |Unblock|)
              (|_Spatial-Entity24462| |instance-of| |Spatial-Entity|)
              (|_Unblock24461| |actions| |_Unblock24461|)
              (|_Unblock24461| |preparatory-event| |_Move24467|)
              (|_Unblock24461| |preparatory-event| |_Block24465|)
              (|_Unblock24461| |primitive-actions| |_Unblock24461|)
              (|_Unblock24461| |time-during| |_Time-Interval24464|)
              (|_Unblock24461| |object| |_Spatial-Entity24462|))
             T NIL)))
(|Airplane| (|_Airplane34593|
             (((|_Airplane34593| |instance-of| |Airplane|)
               (|_Vehicle34595| |instance-of| |Vehicle|)
               (|_Airplane34593| |purpose| |_Vehicle34595|))
              T NIL)))
(|Convey| (|_Convey25839|
           (((|_Move25928| |instance-of| |Move|)
             (|_Time-Interval25927| |instance-of| |Time-Interval|)
             (|_Signal25890| |instance-of| |Signal|)
             (|_Move25877| |instance-of| |Move|)
             (|_Time-Interval25876| |instance-of| |Time-Interval|)
             (|_Move25854| |instance-of| |Move|)
             (|_Time-Interval25853| |instance-of| |Time-Interval|)
             (|_Tangible-Entity25852| |instance-of| |Tangible-Entity|)
             (|_Message25848| |instance-of| |Message|)
             (|_Message25848| |instance-of| |Spatial-Entity|)
             (|_Tangible-Entity25846| |instance-of| |Tangible-Entity|)
             (|_Sense25851| |instance-of| |Sense|)
             (|_Transmit25850| |instance-of| |Transmit|)
             (|_Embody25849| |instance-of| |Embody|)
             (|_Convey25839| |instance-of| |Convey|)
             (|_Tangible-Entity25845| |instance-of| |Tangible-Entity|)
             (|_Sense25851| |actions-of| |_Sense25851|)
             (|_Sense25851| |preparatory-event| |_Move25928|)
             (|_Sense25851| |primitive-actions-of| |_Sense25851|)
             (|_Sense25851| |time-during| |_Time-Interval25927|)
             (|_Sense25851| |experiencer| |_Tangible-Entity25846|)
             (|_Sense25851| |object| |_Tangible-Entity25852|)
             (|_Sense25851| |result| |_Message25848|)
             (|_Sense25851| |agent| |_Tangible-Entity25846|)
             (|_Tangible-Entity25852| |object-of| |_Transmit25850|)
             (|_Tangible-Entity25852| |plays| |_Signal25890|)
             (|_Embody25849| |actions-of| |_Embody25849|)
             (|_Embody25849| |preparatory-event| |_Move25877|)
             (|_Embody25849| |primitive-actions-of| |_Embody25849|)
             (|_Embody25849| |time-during| |_Time-Interval25876|)
             (|_Embody25849| |object| |_Message25848|)
             (|_Embody25849| |result| |_Tangible-Entity25852|)
             (|_Embody25849| |next-event| |_Transmit25850|)
             (|_Embody25849| |agent| |_Tangible-Entity25845|)
             (|_Convey25839| |actions| |_Convey25839|)
             (|_Convey25839| |actions| |_Sense25851|)
             (|_Convey25839| |actions| |_Transmit25850|)
             (|_Convey25839| |actions| |_Embody25849|)
             (|_Convey25839| |all-subevents| |_Sense25851|)
             (|_Convey25839| |all-subevents| |_Transmit25850|)
             (|_Convey25839| |all-subevents| |_Embody25849|)
             (|_Convey25839| |preparatory-event| |_Move25854|)
             (|_Convey25839| |primitive-actions| |_Sense25851|)
             (|_Convey25839| |primitive-actions| |_Transmit25850|)
             (|_Convey25839| |primitive-actions| |_Embody25849|)
             (|_Convey25839| |time-during| |_Time-Interval25853|)
             (|_Convey25839| |base| |_Tangible-Entity25852|)
             (|_Convey25839| |object| |_Message25848|)
             (|_Convey25839| |recipient| |_Tangible-Entity25846|)
             (|_Convey25839| |first-subevent| |_Embody25849|)
             (|_Convey25839| |subevent| |_Sense25851|)
             (|_Convey25839| |subevent| |_Transmit25850|)
             (|_Convey25839| |subevent| |_Embody25849|)
             (|_Convey25839| |agent| |_Tangible-Entity25845|))
            T NIL)))
(|Discovery| (|_Discovery884|
              (((|_Discovery884| |instance-of| |Discovery|)
                (|_Time-Interval890| |instance-of| |Time-Interval|)
                (|_Discovery884| |actions| |_Discovery884|)
                (|_Discovery884| |primitive-actions| |_Discovery884|)
                (|_Discovery884| |time-during| |_Time-Interval890|))
               T NIL)))
(|Shipping| (|_Shipping433|
             (((|_Shipping433| |instance-of| |Shipping|)
               (|_Time-Interval439| |instance-of| |Time-Interval|)
               (|_Shipping433| |actions| |_Shipping433|)
               (|_Shipping433| |primitive-actions| |_Shipping433|)
               (|_Shipping433| |time-during| |_Time-Interval439|))
              T NIL)))
(|UoM-Luminous-Intensity| (|_UoM-Luminous-Intensity37454|
                           (((|_UoM-Luminous-Intensity37454|
                              |instance-of| |UoM-Luminous-Intensity|))
                            T NIL)))
(|Roof| (|_Roof34411|
         (((|_Time-Interval34427| |instance-of| |Time-Interval|)
           (|_Time-Interval34424| |instance-of| |Time-Interval|)
           (|_Time-Interval34421| |instance-of| |Time-Interval|)
           (|_Be-Stable34418| |instance-of| |Be-Stable|)
           (|_Be-Supported34417| |instance-of| |Be-Supported|)
           (|_Cover34416| |instance-of| |Cover|)
           (|_Create34415| |instance-of| |Create|)
           (|_Roof34411| |instance-of| |Roof|)
           (|_Architectural-Structure34414| |instance-of|
            |Architectural-Structure|)
           (|_Be-Stable34418| |actions| |_Be-Stable34418|)
           (|_Be-Stable34418| |primitive-actions| |_Be-Stable34418|)
           (|_Be-Stable34418| |time-during| |_Time-Interval34427|)
           (|_Be-Supported34417| |actions| |_Be-Supported34417|)
           (|_Be-Supported34417| |primitive-actions|
            |_Be-Supported34417|)
           (|_Be-Supported34417| |time-during| |_Time-Interval34424|)
           (|_Create34415| |actions| |_Create34415|)
           (|_Create34415| |primitive-actions| |_Create34415|)
           (|_Create34415| |time-during| |_Time-Interval34421|)
           (|_Roof34411| |is-above| |_Architectural-Structure34414|)
           (|_Roof34411| |object-of| |_Be-Stable34418|)
           (|_Roof34411| |object-of| |_Be-Supported34417|)
           (|_Roof34411| |plays| |_Cover34416|)
           (|_Roof34411| |result-of| |_Create34415|)
           (|_Roof34411| |is-part-of| |_Architectural-Structure34414|))
          T NIL)))
(|Volume-Constant| (|_Volume-Constant37805|
                    (((|_Volume-Constant37805| |instance-of|
                       |Volume-Constant|))
                     T NIL)))
(|Region| (|_Region34673|
           (((|_Region34673| |instance-of| |Region|)) T NIL)))
(|Trait-Constant| (|_Trait-Constant37813|
                   (((|_Trait-Constant37813| |instance-of|
                      |Trait-Constant|))
                    T NIL)))
(|Exert-Force| (|_Exert-Force32895|
                (((|_Exert-Force32895| |instance-of| |Exert-Force|)
                  (|_Time-Interval32901| |instance-of| |Time-Interval|)
                  (|_Exert-Force32895| |actions| |_Exert-Force32895|)
                  (|_Exert-Force32895| |primitive-actions|
                   |_Exert-Force32895|)
                  (|_Exert-Force32895| |time-during|
                   |_Time-Interval32901|))
                 T NIL)))
(|Unitless-Value| (|_Unitless-Value37679|
                   (((|_Unitless-Value37679| |instance-of|
                      |Unitless-Value|))
                    T NIL)))
(|Participant-Relation| (|_Participant-Relation37945|
                         (((|_Participant-Relation37945| |instance-of|
                            |Participant-Relation|))
                          T NIL)))
(|Polygon| (|_Polygon34712|
            (((|_Polygon34712| |instance-of| |Polygon|)) T NIL)))
(|Cardinal| (|_Cardinal37935|
             (((|_Cardinal37935| |instance-of| |Cardinal|)) T NIL)))
(|Integer| (|_Integer33600|
            (((|_Integer33600| |instance-of| |Integer|)) T NIL)))
(|Square| (|_Square34718|
           (((|_Square34718| |instance-of| |Square|)) T NIL)))
(|Dive| (|_Dive24368|
         (((|_Time-Interval24428| |instance-of| |Time-Interval|)
           (|_Entity24424| |instance-of| |Entity|)
           (|_Make-Accessible24422| |instance-of| |Make-Accessible|)
           (|_Time-Interval24421| |instance-of| |Time-Interval|)
           (|_Be-Inaccessible24420| |instance-of| |Be-Inaccessible|)
           (|_Make-Inaccessible24382| |instance-of|
            |Make-Inaccessible|)
           (|_Time-Interval24381| |instance-of| |Time-Interval|)
           (|_Fall24380| |instance-of| |Fall|)
           (|_Propel24376| |instance-of| |Propel|)
           (|_Dive24368| |instance-of| |Dive|)
           (|_Animal24375| |instance-of| |Animal|)
           (|_Be-Inaccessible24420| |actions| |_Be-Inaccessible24420|)
           (|_Be-Inaccessible24420| |primitive-actions|
            |_Be-Inaccessible24420|)
           (|_Be-Inaccessible24420| |time-during|
            |_Time-Interval24428|)
           (|_Be-Inaccessible24420| |object| |_Entity24424|)
           (|_Make-Inaccessible24382| |actions|
            |_Make-Inaccessible24382|)
           (|_Make-Inaccessible24382| |preparatory-event|
            |_Make-Accessible24422|)
           (|_Make-Inaccessible24382| |primitive-actions|
            |_Make-Inaccessible24382|)
           (|_Make-Inaccessible24382| |time-during|
            |_Time-Interval24421|)
           (|_Make-Inaccessible24382| |object| |_Animal24375|)
           (|_Make-Inaccessible24382| |resulting-state|
            |_Be-Inaccessible24420|)
           (|_Make-Inaccessible24382| |agent| |_Animal24375|)
           (|_Dive24368| |actions| |_Dive24368|)
           (|_Dive24368| |preparatory-event| |_Make-Inaccessible24382|)
           (|_Dive24368| |primitive-actions| |_Dive24368|)
           (|_Dive24368| |time-during| |_Time-Interval24381|)
           (|_Dive24368| |object| |_Animal24375|)
           (|_Dive24368| |causes| |_Fall24380|)
           (|_Dive24368| |caused-by| |_Propel24376|)
           (|_Dive24368| |agent| |_Animal24375|))
          T NIL)))
(|Block| (|_Block23950|
          (((|_Time-Interval23963| |instance-of| |Time-Interval|)
            (|_Spatial-Entity23959| |instance-of| |Spatial-Entity|)
            (|_Move23957| |instance-of| |Move|)
            (|_Unblock23955| |instance-of| |Unblock|)
            (|_Time-Interval23954| |instance-of| |Time-Interval|)
            (|_Spatial-Entity23951| |instance-of| |Spatial-Entity|)
            (|_Block23950| |instance-of| |Block|)
            (|_Be-Blocked23953| |instance-of| |Be-Blocked|)
            (|_Be-Blocked23953| |actions| |_Be-Blocked23953|)
            (|_Be-Blocked23953| |primitive-actions| |_Be-Blocked23953|)
            (|_Be-Blocked23953| |time-during| |_Time-Interval23963|)
            (|_Be-Blocked23953| |object| |_Spatial-Entity23959|)
            (|_Block23950| |actions| |_Block23950|)
            (|_Block23950| |preparatory-event| |_Move23957|)
            (|_Block23950| |preparatory-event| |_Unblock23955|)
            (|_Block23950| |primitive-actions| |_Block23950|)
            (|_Block23950| |time-during| |_Time-Interval23954|)
            (|_Block23950| |object| |_Spatial-Entity23951|)
            (|_Block23950| |resulting-state| |_Be-Blocked23953|))
           T NIL)))
(|Invade| (|_Invade37332|
           (((|_Entity37402| |instance-of| |Entity|)
             (|_Leave37379| |instance-of| |Leave|)
             (|_Time-Interval37378| |instance-of| |Time-Interval|)
             (|_Spatial-Entity37349| |instance-of| |Spatial-Entity|)
             (|_Event37377| |instance-of| |Event|)
             (|_Event37376| |instance-of| |Event|)
             (|_Tangible-Entity37348| |instance-of| |Tangible-Entity|)
             (|_Acceleration-Magnitude-Value37352| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Acceleration-Vector-Value37351| |instance-of|
              |Acceleration-Vector-Value|)
             (|_Length-Value37358| |instance-of| |Length-Value|)
             (|_Duration-Value37350| |instance-of| |Duration-Value|)
             (|_Speed-Value37354| |instance-of| |Speed-Value|)
             (|_Displacement-Vector-Value37357| |instance-of|
              |Displacement-Vector-Value|)
             (|_Speed-Value37375| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value37372| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value37374| |instance-of| |Speed-Value|)
             (|_Speed-Value37373| |instance-of| |Speed-Value|)
             (|_Speed-Value37371| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value37368| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value37370| |instance-of| |Speed-Value|)
             (|_Speed-Value37369| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value37353| |instance-of|
              |Velocity-Vector-Value|)
             (|_Acceleration-Magnitude-Value37367| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value37366| |instance-of| |Length-Value|)
             (|_Speed-Value37365| |instance-of| |Speed-Value|)
             (|_Acceleration-Magnitude-Value37364| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value37363| |instance-of| |Length-Value|)
             (|_Invade37332| |instance-of| |Invade|)
             (|_Speed-Value37362| |instance-of| |Speed-Value|)
             (|_Spatial-Entity37349| |is-possessed-by| |_Entity37402|)
             (|_Invade37332| |actions| |_Invade37332|)
             (|_Invade37332| |preparatory-event| |_Leave37379|)
             (|_Invade37332| |primitive-actions| |_Invade37332|)
             (|_Invade37332| |time-during| |_Time-Interval37378|)
             (|_Invade37332| |destination| |_Spatial-Entity37349|)
             (|_Invade37332| |object| |_Tangible-Entity37348|)
             (|_Invade37332| |inhibited-by| |_Event37376|)
             (|_Invade37332| |objective| |_Event37377|)
             (|_Invade37332| |prevented-by| |_Event37376|)
             (|_Invade37332| |agent| |_Tangible-Entity37348|)
             (|_Invade37332| |acceleration-magnitude|
              |_Acceleration-Magnitude-Value37352|)
             (|_Invade37332| |acceleration|
              |_Acceleration-Vector-Value37351|)
             (|_Invade37332| |distance| |_Length-Value37358|)
             (|_Invade37332| |duration| |_Duration-Value37350|)
             (|_Invade37332| |speed| |_Speed-Value37354|)
             (|_Invade37332| |displacement|
              |_Displacement-Vector-Value37357|)
             (|_Invade37332| |final-speed| |_Speed-Value37375|)
             (|_Invade37332| |final-velocity|
              |_Velocity-Vector-Value37372|)
             (|_Invade37332| |final-x-speed| |_Speed-Value37374|)
             (|_Invade37332| |final-y-speed| |_Speed-Value37373|)
             (|_Invade37332| |initial-speed| |_Speed-Value37371|)
             (|_Invade37332| |initial-velocity|
              |_Velocity-Vector-Value37368|)
             (|_Invade37332| |initial-x-speed| |_Speed-Value37370|)
             (|_Invade37332| |initial-y-speed| |_Speed-Value37369|)
             (|_Invade37332| |velocity| |_Velocity-Vector-Value37353|)
             (|_Invade37332| |x-acceleration-magnitude|
              |_Acceleration-Magnitude-Value37367|)
             (|_Invade37332| |x-distance| |_Length-Value37366|)
             (|_Invade37332| |x-speed| |_Speed-Value37365|)
             (|_Invade37332| |y-acceleration-magnitude|
              |_Acceleration-Magnitude-Value37364|)
             (|_Invade37332| |y-distance| |_Length-Value37363|)
             (|_Invade37332| |y-speed| |_Speed-Value37362|))
            T NIL)))
(|Give| (|_Give1752|
         (((|_Move1771| |instance-of| |Move|)
           (|_Obtain1770| |instance-of| |Obtain|)
           (|_Time-Interval1769| |instance-of| |Time-Interval|)
           (|_Move1764| |instance-of| |Move|)
           (|_Obtain1763| |instance-of| |Obtain|)
           (|_Time-Interval1762| |instance-of| |Time-Interval|)
           (|_Entity1761| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity1759| |instance-of| |Tangible-Entity|)
           (|_Give1752| |instance-of| |Give|)
           (|_Tangible-Entity1758| |instance-of| |Tangible-Entity|)
           (|_Obtain1763| |actions| |_Obtain1763|)
           (|_Obtain1763| |preparatory-event| |_Move1771|)
           (|_Obtain1763| |preparatory-event| |_Obtain1770|)
           (|_Obtain1763| |primitive-actions| |_Obtain1763|)
           (|_Obtain1763| |time-during| |_Time-Interval1769|)
           (|_Obtain1763| |object| |_Entity1761|)
           (|_Obtain1763| |recipient| |_Tangible-Entity1758|)
           (|_Obtain1763| |agent| |_Tangible-Entity1758|)
           (|_Give1752| |actions| |_Give1752|)
           (|_Give1752| |preparatory-event| |_Move1764|)
           (|_Give1752| |preparatory-event| |_Obtain1763|)
           (|_Give1752| |primitive-actions| |_Give1752|)
           (|_Give1752| |time-during| |_Time-Interval1762|)
           (|_Give1752| |object| |_Entity1761|)
           (|_Give1752| |recipient| |_Tangible-Entity1759|)
           (|_Give1752| |agent| |_Tangible-Entity1758|)
           (|_Give1752| |donor| |_Tangible-Entity1758|))
          T NIL)))
(|Quantity-Constant| (|_Quantity-Constant37845|
                      (((|_Quantity-Constant37845| |instance-of|
                         |Quantity-Constant|))
                       T NIL)))
(|Financing| (|_Financing848|
              (((|_Financing848| |instance-of| |Financing|)
                (|_Time-Interval854| |instance-of| |Time-Interval|)
                (|_Financing848| |actions| |_Financing848|)
                (|_Financing848| |primitive-actions| |_Financing848|)
                (|_Financing848| |time-during| |_Time-Interval854|))
               T NIL)))
(|Withdraw| (|_Withdraw19962|
             (((|_Move20062| |instance-of| |Move|)
               (|_Time-Interval20061| |instance-of| |Time-Interval|)
               (|_Tangible-Entity20060| |instance-of|
                |Tangible-Entity|)
               (|_Acceleration-Magnitude-Value20059| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Acceleration-Vector-Value20043| |instance-of|
                |Acceleration-Vector-Value|)
               (|_Length-Value20058| |instance-of| |Length-Value|)
               (|_Duration-Value20057| |instance-of| |Duration-Value|)
               (|_Speed-Value20056| |instance-of| |Speed-Value|)
               (|_Displacement-Vector-Value20041| |instance-of|
                |Displacement-Vector-Value|)
               (|_Speed-Value20055| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value20052| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value20054| |instance-of| |Speed-Value|)
               (|_Speed-Value20053| |instance-of| |Speed-Value|)
               (|_Speed-Value20051| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value20048| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value20050| |instance-of| |Speed-Value|)
               (|_Speed-Value20049| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value20039| |instance-of|
                |Velocity-Vector-Value|)
               (|_Acceleration-Magnitude-Value20047| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value20046| |instance-of| |Length-Value|)
               (|_Speed-Value20045| |instance-of| |Speed-Value|)
               (|_Acceleration-Magnitude-Value20044| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value20042| |instance-of| |Length-Value|)
               (|_Speed-Value20040| |instance-of| |Speed-Value|)
               (|_Move20038| |instance-of| |Move|)
               (|_Time-Interval20037| |instance-of| |Time-Interval|)
               (|_Acceleration-Magnitude-Value20036| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Acceleration-Vector-Value20020| |instance-of|
                |Acceleration-Vector-Value|)
               (|_Length-Value20035| |instance-of| |Length-Value|)
               (|_Duration-Value20034| |instance-of| |Duration-Value|)
               (|_Speed-Value20033| |instance-of| |Speed-Value|)
               (|_Displacement-Vector-Value20018| |instance-of|
                |Displacement-Vector-Value|)
               (|_Speed-Value20032| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value20029| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value20031| |instance-of| |Speed-Value|)
               (|_Speed-Value20030| |instance-of| |Speed-Value|)
               (|_Speed-Value20028| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value20025| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value20027| |instance-of| |Speed-Value|)
               (|_Speed-Value20026| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value20016| |instance-of|
                |Velocity-Vector-Value|)
               (|_Acceleration-Magnitude-Value20024| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value20023| |instance-of| |Length-Value|)
               (|_Speed-Value20022| |instance-of| |Speed-Value|)
               (|_Acceleration-Magnitude-Value20021| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value20019| |instance-of| |Length-Value|)
               (|_Speed-Value20017| |instance-of| |Speed-Value|)
               (|_Move20015| |instance-of| |Move|)
               (|_Admit20013| |instance-of| |Admit|)
               (|_Time-Interval20012| |instance-of| |Time-Interval|)
               (|_Be-Shut-Out20011| |instance-of| |Be-Shut-Out|)
               (|_Move20009| |instance-of| |Move|)
               (|_Withdraw20008| |instance-of| |Withdraw|)
               (|_Time-Interval20007| |instance-of| |Time-Interval|)
               (|_Tangible-Entity20006| |instance-of|
                |Tangible-Entity|)
               (|_Be-Stored20005| |instance-of| |Be-Stored|)
               (|_Portal20004| |instance-of| |Portal|)
               (|_Barrier20003| |instance-of| |Container|)
               (|_Angle-Value20002| |instance-of| |Angle-Value|)
               (|_Angle-Value20001| |instance-of| |Angle-Value|)
               (|_Angle-Value20000| |instance-of| |Angle-Value|)
               (|_Angle-Value19999| |instance-of| |Angle-Value|)
               (|_Angle-Value19998| |instance-of| |Angle-Value|)
               (|_Move19994| |instance-of| |Move|)
               (|_Move19993| |instance-of| |Move|)
               (|_Shut-Out19992| |instance-of| |Shut-Out|)
               (|_Store19991| |instance-of| |Store|)
               (|_Time-Interval19990| |instance-of| |Time-Interval|)
               (|_Spatial-Entity19987| |instance-of| |Spatial-Entity|)
               (|_Spatial-Entity19985| |instance-of| |Spatial-Entity|)
               (|_Spatial-Entity19988| |instance-of| |Spatial-Entity|)
               (|_Tangible-Entity19984| |instance-of|
                |Tangible-Entity|)
               (|_Entity19986| |instance-of| |Tangible-Entity|)
               (|_Acceleration-Magnitude-Value19983| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Acceleration-Vector-Value19967| |instance-of|
                |Acceleration-Vector-Value|)
               (|_Length-Value19982| |instance-of| |Length-Value|)
               (|_Duration-Value19981| |instance-of| |Duration-Value|)
               (|_Speed-Value19980| |instance-of| |Speed-Value|)
               (|_Displacement-Vector-Value19965| |instance-of|
                |Displacement-Vector-Value|)
               (|_Speed-Value19979| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value19976| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value19978| |instance-of| |Speed-Value|)
               (|_Speed-Value19977| |instance-of| |Speed-Value|)
               (|_Speed-Value19975| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value19972| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value19974| |instance-of| |Speed-Value|)
               (|_Speed-Value19973| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value19963| |instance-of|
                |Velocity-Vector-Value|)
               (|_Acceleration-Magnitude-Value19971| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value19970| |instance-of| |Length-Value|)
               (|_Speed-Value19969| |instance-of| |Speed-Value|)
               (|_Acceleration-Magnitude-Value19968| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value19966| |instance-of| |Length-Value|)
               (|_Withdraw19962| |instance-of| |Withdraw|)
               (|_Speed-Value19964| |instance-of| |Speed-Value|)
               (|_Move19994| |actions| |_Move19994|)
               (|_Move19994| |preparatory-event| |_Move20062|)
               (|_Move19994| |primitive-actions| |_Move19994|)
               (|_Move19994| |time-during| |_Time-Interval20061|)
               (|_Move19994| |object| |_Tangible-Entity20060|)
               (|_Move19994| |acceleration-magnitude|
                |_Acceleration-Magnitude-Value20059|)
               (|_Move19994| |acceleration|
                |_Acceleration-Vector-Value20043|)
               (|_Move19994| |distance| |_Length-Value20058|)
               (|_Move19994| |duration| |_Duration-Value20057|)
               (|_Move19994| |speed| |_Speed-Value20056|)
               (|_Move19994| |displacement|
                |_Displacement-Vector-Value20041|)
               (|_Move19994| |final-speed| |_Speed-Value20055|)
               (|_Move19994| |final-velocity|
                |_Velocity-Vector-Value20052|)
               (|_Move19994| |final-x-speed| |_Speed-Value20054|)
               (|_Move19994| |final-y-speed| |_Speed-Value20053|)
               (|_Move19994| |initial-speed| |_Speed-Value20051|)
               (|_Move19994| |initial-velocity|
                |_Velocity-Vector-Value20048|)
               (|_Move19994| |initial-x-speed| |_Speed-Value20050|)
               (|_Move19994| |initial-y-speed| |_Speed-Value20049|)
               (|_Move19994| |velocity| |_Velocity-Vector-Value20039|)
               (|_Move19994| |x-acceleration-magnitude|
                |_Acceleration-Magnitude-Value20047|)
               (|_Move19994| |x-distance| |_Length-Value20046|)
               (|_Move19994| |x-speed| |_Speed-Value20045|)
               (|_Move19994| |y-acceleration-magnitude|
                |_Acceleration-Magnitude-Value20044|)
               (|_Move19994| |y-distance| |_Length-Value20042|)
               (|_Move19994| |y-speed| |_Speed-Value20040|)
               (|_Move19993| |actions| |_Move19993|)
               (|_Move19993| |preparatory-event| |_Move20038|)
               (|_Move19993| |primitive-actions| |_Move19993|)
               (|_Move19993| |time-during| |_Time-Interval20037|)
               (|_Move19993| |destination| |_Spatial-Entity19985|)
               (|_Move19993| |object| |_Entity19986|)
               (|_Move19993| |acceleration-magnitude|
                |_Acceleration-Magnitude-Value20036|)
               (|_Move19993| |acceleration|
                |_Acceleration-Vector-Value20020|)
               (|_Move19993| |distance| |_Length-Value20035|)
               (|_Move19993| |duration| |_Duration-Value20034|)
               (|_Move19993| |speed| |_Speed-Value20033|)
               (|_Move19993| |displacement|
                |_Displacement-Vector-Value20018|)
               (|_Move19993| |final-speed| |_Speed-Value20032|)
               (|_Move19993| |final-velocity|
                |_Velocity-Vector-Value20029|)
               (|_Move19993| |final-x-speed| |_Speed-Value20031|)
               (|_Move19993| |final-y-speed| |_Speed-Value20030|)
               (|_Move19993| |initial-speed| |_Speed-Value20028|)
               (|_Move19993| |initial-velocity|
                |_Velocity-Vector-Value20025|)
               (|_Move19993| |initial-x-speed| |_Speed-Value20027|)
               (|_Move19993| |initial-y-speed| |_Speed-Value20026|)
               (|_Move19993| |velocity| |_Velocity-Vector-Value20016|)
               (|_Move19993| |x-acceleration-magnitude|
                |_Acceleration-Magnitude-Value20024|)
               (|_Move19993| |x-distance| |_Length-Value20023|)
               (|_Move19993| |x-speed| |_Speed-Value20022|)
               (|_Move19993| |y-acceleration-magnitude|
                |_Acceleration-Magnitude-Value20021|)
               (|_Move19993| |y-distance| |_Length-Value20019|)
               (|_Move19993| |y-speed| |_Speed-Value20017|)
               (|_Shut-Out19992| |actions| |_Shut-Out19992|)
               (|_Shut-Out19992| |preparatory-event| |_Move20015|)
               (|_Shut-Out19992| |preparatory-event| |_Admit20013|)
               (|_Shut-Out19992| |primitive-actions| |_Shut-Out19992|)
               (|_Shut-Out19992| |time-during| |_Time-Interval20012|)
               (|_Shut-Out19992| |base| |_Tangible-Entity19984|)
               (|_Shut-Out19992| |object| |_Entity19986|)
               (|_Shut-Out19992| |resulting-state| |_Be-Shut-Out20011|)
               (|_Store19991| |actions| |_Store19991|)
               (|_Store19991| |preparatory-event| |_Move20009|)
               (|_Store19991| |preparatory-event| |_Withdraw20008|)
               (|_Store19991| |primitive-actions| |_Store19991|)
               (|_Store19991| |time-during| |_Time-Interval20007|)
               (|_Store19991| |base| |_Tangible-Entity20006|)
               (|_Store19991| |object| |_Entity19986|)
               (|_Store19991| |resulting-state| |_Be-Stored20005|)
               (|_Spatial-Entity19987| |is-outside|
                |_Tangible-Entity19984|)
               (|_Spatial-Entity19988| |plays| |_Portal20004|)
               (|_Tangible-Entity19984| |encloses|
                |_Spatial-Entity19985|)
               (|_Tangible-Entity19984| |plays| |_Barrier20003|)
               (|_Tangible-Entity19984| |has-region|
                |_Spatial-Entity19988|)
               (|_Entity19986| |destination-of| |_Move19994|)
               (|_Acceleration-Vector-Value19967| |x-component-slot|
                |x-acceleration-magnitude|)
               (|_Acceleration-Vector-Value19967| |y-component-slot|
                |y-acceleration-magnitude|)
               (|_Acceleration-Vector-Value19967|
                |acceleration-magnitude|
                |_Acceleration-Magnitude-Value19983|)
               (|_Acceleration-Vector-Value19967| |direction|
                |_Angle-Value20002|)
               (|_Displacement-Vector-Value19965| |x-component-slot|
                |x-distance|)
               (|_Displacement-Vector-Value19965| |y-component-slot|
                |y-distance|)
               (|_Displacement-Vector-Value19965| |direction|
                |_Angle-Value20001|)
               (|_Displacement-Vector-Value19965| |distance|
                |_Length-Value19982|)
               (|_Velocity-Vector-Value19976| |x-component-slot|
                |x-speed|)
               (|_Velocity-Vector-Value19976| |y-component-slot|
                |y-speed|)
               (|_Velocity-Vector-Value19976| |direction|
                |_Angle-Value20000|)
               (|_Velocity-Vector-Value19976| |speed|
                |_Speed-Value19979|)
               (|_Speed-Value19978| |x-speed-of|
                |_Velocity-Vector-Value19976|)
               (|_Speed-Value19977| |y-speed-of|
                |_Velocity-Vector-Value19976|)
               (|_Velocity-Vector-Value19972| |x-component-slot|
                |x-speed|)
               (|_Velocity-Vector-Value19972| |y-component-slot|
                |y-speed|)
               (|_Velocity-Vector-Value19972| |direction|
                |_Angle-Value19999|)
               (|_Velocity-Vector-Value19972| |speed|
                |_Speed-Value19975|)
               (|_Speed-Value19974| |x-speed-of|
                |_Velocity-Vector-Value19972|)
               (|_Speed-Value19973| |y-speed-of|
                |_Velocity-Vector-Value19972|)
               (|_Velocity-Vector-Value19963| |x-component-slot|
                |x-speed|)
               (|_Velocity-Vector-Value19963| |y-component-slot|
                |y-speed|)
               (|_Velocity-Vector-Value19963| |direction|
                |_Angle-Value19998|)
               (|_Velocity-Vector-Value19963| |speed|
                |_Speed-Value19980|)
               (|_Acceleration-Magnitude-Value19971|
                |x-acceleration-magnitude-of|
                |_Acceleration-Vector-Value19967|)
               (|_Length-Value19970| |x-distance-of|
                |_Displacement-Vector-Value19965|)
               (|_Speed-Value19969| |x-speed-of|
                |_Velocity-Vector-Value19963|)
               (|_Acceleration-Magnitude-Value19968|
                |y-acceleration-magnitude-of|
                |_Acceleration-Vector-Value19967|)
               (|_Length-Value19966| |y-distance-of|
                |_Displacement-Vector-Value19965|)
               (|_Speed-Value19964| |y-speed-of|
                |_Velocity-Vector-Value19963|)
               (|_Withdraw19962| |actions| |_Withdraw19962|)
               (|_Withdraw19962| |preparatory-event| |_Move19994|)
               (|_Withdraw19962| |preparatory-event| |_Move19993|)
               (|_Withdraw19962| |preparatory-event| |_Shut-Out19992|)
               (|_Withdraw19962| |preparatory-event| |_Store19991|)
               (|_Withdraw19962| |primitive-actions| |_Withdraw19962|)
               (|_Withdraw19962| |time-during| |_Time-Interval19990|)
               (|_Withdraw19962| |destination| |_Spatial-Entity19987|)
               (|_Withdraw19962| |origin| |_Spatial-Entity19985|)
               (|_Withdraw19962| |path| |_Spatial-Entity19988|)
               (|_Withdraw19962| |base| |_Tangible-Entity19984|)
               (|_Withdraw19962| |object| |_Entity19986|)
               (|_Withdraw19962| |acceleration-magnitude|
                |_Acceleration-Magnitude-Value19983|)
               (|_Withdraw19962| |acceleration|
                |_Acceleration-Vector-Value19967|)
               (|_Withdraw19962| |distance| |_Length-Value19982|)
               (|_Withdraw19962| |duration| |_Duration-Value19981|)
               (|_Withdraw19962| |speed| |_Speed-Value19980|)
               (|_Withdraw19962| |displacement|
                |_Displacement-Vector-Value19965|)
               (|_Withdraw19962| |final-speed| |_Speed-Value19979|)
               (|_Withdraw19962| |final-velocity|
                |_Velocity-Vector-Value19976|)
               (|_Withdraw19962| |final-x-speed| |_Speed-Value19978|)
               (|_Withdraw19962| |final-y-speed| |_Speed-Value19977|)
               (|_Withdraw19962| |initial-speed| |_Speed-Value19975|)
               (|_Withdraw19962| |initial-velocity|
                |_Velocity-Vector-Value19972|)
               (|_Withdraw19962| |initial-x-speed| |_Speed-Value19974|)
               (|_Withdraw19962| |initial-y-speed| |_Speed-Value19973|)
               (|_Withdraw19962| |velocity|
                |_Velocity-Vector-Value19963|)
               (|_Withdraw19962| |x-acceleration-magnitude|
                |_Acceleration-Magnitude-Value19971|)
               (|_Withdraw19962| |x-distance| |_Length-Value19970|)
               (|_Withdraw19962| |x-speed| |_Speed-Value19969|)
               (|_Withdraw19962| |y-acceleration-magnitude|
                |_Acceleration-Magnitude-Value19968|)
               (|_Withdraw19962| |y-distance| |_Length-Value19966|)
               (|_Withdraw19962| |y-speed| |_Speed-Value19964|))
              NIL NIL)))
(|Replication| (|_Replication489|
                (((|_Replication489| |instance-of| |Replication|)
                  (|_Time-Interval495| |instance-of| |Time-Interval|)
                  (|_Replication489| |actions| |_Replication489|)
                  (|_Replication489| |primitive-actions|
                   |_Replication489|)
                  (|_Replication489| |time-during|
                   |_Time-Interval495|))
                 T NIL)))
(|Integrity-Scale| (|_Integrity-Scale37577|
                    (((|_Integrity-Scale37577| |instance-of|
                       |Integrity-Scale|)
                      (|_Number37579| |instance-of| |Number|)
                      (|_Integrity-Scale37577| |number-of-elements|
                       |_Number37579|))
                     T NIL)))
(|Authorized| (|_Authorized37418|
               (((|_Authorized37418| |instance-of| |Authorized|)) T
                NIL)))
(|Organism| (|_Organism34014|
             (((|_Organism34014| |instance-of| |Organism|)) T NIL)))
(|Day-Constant| (|_Day-Constant37901|
                 (((|_Day-Constant37901| |instance-of| |Day-Constant|))
                  T NIL)))
(|Brightness-Value| (|_Brightness-Value37753|
                     (((|_Brightness-Value37753| |instance-of|
                        |Brightness-Value|))
                      T NIL)))
(|Fidget| (|_Fidget10307|
           (((|_Time-Interval10313| |instance-of| |Time-Interval|)
             (|_Fidget10307| |instance-of| |Fidget|)
             (|_Entity10311| |instance-of| |Tangible-Entity|)
             (|_Fidget10307| |actions| |_Fidget10307|)
             (|_Fidget10307| |primitive-actions| |_Fidget10307|)
             (|_Fidget10307| |time-during| |_Time-Interval10313|)
             (|_Fidget10307| |object| |_Entity10311|)
             (|_Fidget10307| |agent| |_Entity10311|))
            T NIL)))
(|Force-Vector-Value| (|_Force-Vector-Value37777|
                       (((|_Angle-Value37782| |instance-of|
                          |Angle-Value|)
                         (|_Force-Magnitude-Value37781| |instance-of|
                          |Force-Magnitude-Value|)
                         (|_Force-Magnitude-Value37784| |instance-of|
                          |Force-Magnitude-Value|)
                         (|_Force-Vector-Value37777| |instance-of|
                          |Force-Vector-Value|)
                         (|_Force-Magnitude-Value37783| |instance-of|
                          |Force-Magnitude-Value|)
                         (|_Force-Vector-Value37777| |x-component-slot|
                          |x-force-magnitude|)
                         (|_Force-Vector-Value37777| |y-component-slot|
                          |y-force-magnitude|)
                         (|_Force-Vector-Value37777| |direction|
                          |_Angle-Value37782|)
                         (|_Force-Vector-Value37777| |force-magnitude|
                          |_Force-Magnitude-Value37781|)
                         (|_Force-Vector-Value37777|
                          |x-force-magnitude|
                          |_Force-Magnitude-Value37784|)
                         (|_Force-Vector-Value37777|
                          |y-force-magnitude|
                          |_Force-Magnitude-Value37783|))
                        T NIL)))
(|Organic-Molecule| (|_Organic-Molecule33990|
                     (((|_Organic-Molecule33990| |instance-of|
                        |Organic-Molecule|)
                       (|_Atom33992| |instance-of| |Atom|)
                       (|_Organic-Molecule33990|
                        |has-basic-structural-unit| |_Atom33992|))
                      T NIL)))
(|Remove| (|_Remove9958|
           (((|_Move9966| |instance-of| |Move|)
             (|_Time-Interval9965| |instance-of| |Time-Interval|)
             (|_Tangible-Entity9964| |instance-of| |Tangible-Entity|)
             (|_Remove9958| |instance-of| |Remove|)
             (|_Tangible-Entity9962| |instance-of| |Tangible-Entity|)
             (|_Remove9958| |actions| |_Remove9958|)
             (|_Remove9958| |preparatory-event| |_Move9966|)
             (|_Remove9958| |primitive-actions| |_Remove9958|)
             (|_Remove9958| |time-during| |_Time-Interval9965|)
             (|_Remove9958| |base| |_Tangible-Entity9964|)
             (|_Remove9958| |object| |_Tangible-Entity9962|))
            T NIL)))
(|Circle| (|_Circle34691|
           (((|_Circle34691| |instance-of| |Circle|)) T NIL)))
(|Size-Scale| (|_Size-Scale37532|
               (((|_Size-Scale37532| |instance-of| |Size-Scale|)
                 (|_Number37534| |instance-of| |Number|)
                 (|_Size-Scale37532| |number-of-elements|
                  |_Number37534|))
                T NIL)))
(|Transmit| (|_Transmit10385|
             (((|_Signal10438| |instance-of| |Signal|)
               (|_Move10425| |instance-of| |Move|)
               (|_Time-Interval10424| |instance-of| |Time-Interval|)
               (|_Tangible-Entity10423| |instance-of|
                |Tangible-Entity|)
               (|_Acceleration-Magnitude-Value10400| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Acceleration-Vector-Value10399| |instance-of|
                |Acceleration-Vector-Value|)
               (|_Length-Value10406| |instance-of| |Length-Value|)
               (|_Duration-Value10398| |instance-of| |Duration-Value|)
               (|_Speed-Value10402| |instance-of| |Speed-Value|)
               (|_Displacement-Vector-Value10405| |instance-of|
                |Displacement-Vector-Value|)
               (|_Speed-Value10422| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value10419| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value10421| |instance-of| |Speed-Value|)
               (|_Speed-Value10420| |instance-of| |Speed-Value|)
               (|_Speed-Value10418| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value10415| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value10417| |instance-of| |Speed-Value|)
               (|_Speed-Value10416| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value10401| |instance-of|
                |Velocity-Vector-Value|)
               (|_Acceleration-Magnitude-Value10414| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value10413| |instance-of| |Length-Value|)
               (|_Speed-Value10412| |instance-of| |Speed-Value|)
               (|_Acceleration-Magnitude-Value10411| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value10410| |instance-of| |Length-Value|)
               (|_Transmit10385| |instance-of| |Transmit|)
               (|_Speed-Value10409| |instance-of| |Speed-Value|)
               (|_Tangible-Entity10423| |plays| |_Signal10438|)
               (|_Transmit10385| |actions| |_Transmit10385|)
               (|_Transmit10385| |preparatory-event| |_Move10425|)
               (|_Transmit10385| |primitive-actions| |_Transmit10385|)
               (|_Transmit10385| |time-during| |_Time-Interval10424|)
               (|_Transmit10385| |object| |_Tangible-Entity10423|)
               (|_Transmit10385| |acceleration-magnitude|
                |_Acceleration-Magnitude-Value10400|)
               (|_Transmit10385| |acceleration|
                |_Acceleration-Vector-Value10399|)
               (|_Transmit10385| |distance| |_Length-Value10406|)
               (|_Transmit10385| |duration| |_Duration-Value10398|)
               (|_Transmit10385| |speed| |_Speed-Value10402|)
               (|_Transmit10385| |displacement|
                |_Displacement-Vector-Value10405|)
               (|_Transmit10385| |final-speed| |_Speed-Value10422|)
               (|_Transmit10385| |final-velocity|
                |_Velocity-Vector-Value10419|)
               (|_Transmit10385| |final-x-speed| |_Speed-Value10421|)
               (|_Transmit10385| |final-y-speed| |_Speed-Value10420|)
               (|_Transmit10385| |initial-speed| |_Speed-Value10418|)
               (|_Transmit10385| |initial-velocity|
                |_Velocity-Vector-Value10415|)
               (|_Transmit10385| |initial-x-speed| |_Speed-Value10417|)
               (|_Transmit10385| |initial-y-speed| |_Speed-Value10416|)
               (|_Transmit10385| |velocity|
                |_Velocity-Vector-Value10401|)
               (|_Transmit10385| |x-acceleration-magnitude|
                |_Acceleration-Magnitude-Value10414|)
               (|_Transmit10385| |x-distance| |_Length-Value10413|)
               (|_Transmit10385| |x-speed| |_Speed-Value10412|)
               (|_Transmit10385| |y-acceleration-magnitude|
                |_Acceleration-Magnitude-Value10411|)
               (|_Transmit10385| |y-distance| |_Length-Value10410|)
               (|_Transmit10385| |y-speed| |_Speed-Value10409|))
              T NIL)))
(|Luminous-Flux-Constant| (|_Luminous-Flux-Constant37867|
                           (((|_Luminous-Flux-Constant37867|
                              |instance-of| |Luminous-Flux-Constant|))
                            T NIL)))
(|Defense| (|_Defense929|
            (((|_Defense929| |instance-of| |Defense|)
              (|_Time-Interval935| |instance-of| |Time-Interval|)
              (|_Defense929| |actions| |_Defense929|)
              (|_Defense929| |primitive-actions| |_Defense929|)
              (|_Defense929| |time-during| |_Time-Interval935|))
             T NIL)))
(|Chemical-Entity| (|_Chemical-Entity33984|
                    (((|_Chemical-Entity33984| |instance-of|
                       |Chemical-Entity|))
                     T NIL)))
(|Destruction| (|_Destruction920|
                (((|_Destruction920| |instance-of| |Destruction|)
                  (|_Time-Interval926| |instance-of| |Time-Interval|)
                  (|_Destruction920| |actions| |_Destruction920|)
                  (|_Destruction920| |primitive-actions|
                   |_Destruction920|)
                  (|_Destruction920| |time-during|
                   |_Time-Interval926|))
                 T NIL)))
(|Be-Stable| (|_Be-Stable217|
              (((|_Time-Interval221| |instance-of| |Time-Interval|)
                (|_Be-Stable217| |instance-of| |Be-Stable|)
                (|_Entity220| |instance-of| |Entity|)
                (|_Be-Stable217| |actions| |_Be-Stable217|)
                (|_Be-Stable217| |primitive-actions| |_Be-Stable217|)
                (|_Be-Stable217| |time-during| |_Time-Interval221|)
                (|_Be-Stable217| |object| |_Entity220|))
               T NIL)))
(|Speed-Value| (|_Speed-Value37728|
                (((|_Speed-Value37728| |instance-of| |Speed-Value|)) T
                 NIL)))
(|Maintenance| (|_Maintenance749|
                (((|_Maintenance749| |instance-of| |Maintenance|)
                  (|_Time-Interval755| |instance-of| |Time-Interval|)
                  (|_Maintenance749| |actions| |_Maintenance749|)
                  (|_Maintenance749| |primitive-actions|
                   |_Maintenance749|)
                  (|_Maintenance749| |time-during|
                   |_Time-Interval755|))
                 T NIL)))
(|Selection| (|_Selection462|
              (((|_Selection462| |instance-of| |Selection|)
                (|_Time-Interval468| |instance-of| |Time-Interval|)
                (|_Selection462| |actions| |_Selection462|)
                (|_Selection462| |primitive-actions| |_Selection462|)
                (|_Selection462| |time-during| |_Time-Interval468|))
               T NIL)))
(|Bird| (|_Bird34033| (((|_Bird34033| |instance-of| |Bird|)) T NIL)))
(|Luminance-Value| (|_Luminance-Value37737|
                    (((|_Luminance-Value37737| |instance-of|
                       |Luminance-Value|))
                     T NIL)))
(|Be-Standing| (|_Be-Standing210|
                (((|_Time-Interval214| |instance-of| |Time-Interval|)
                  (|_Be-Standing210| |instance-of| |Be-Standing|)
                  (|_Entity213| |instance-of| |Entity|)
                  (|_Be-Standing210| |actions| |_Be-Standing210|)
                  (|_Be-Standing210| |primitive-actions|
                   |_Be-Standing210|)
                  (|_Be-Standing210| |time-during| |_Time-Interval214|)
                  (|_Be-Standing210| |object| |_Entity213|))
                 T NIL)))
(|Selling| (|_Selling453|
            (((|_Selling453| |instance-of| |Selling|)
              (|_Time-Interval459| |instance-of| |Time-Interval|)
              (|_Selling453| |actions| |_Selling453|)
              (|_Selling453| |primitive-actions| |_Selling453|)
              (|_Selling453| |time-during| |_Time-Interval459|))
             T NIL)))
(|Ceiling| (|_Ceiling34495|
            (((|_Time-Interval34518| |instance-of| |Time-Interval|)
              (|_Time-Interval34515| |instance-of| |Time-Interval|)
              (|_Time-Interval34512| |instance-of| |Time-Interval|)
              (|_Scalar34499| |instance-of| |Scalar|)
              (|_Be-Stable34503| |instance-of| |Be-Stable|)
              (|_Be-Supported34502| |instance-of| |Be-Supported|)
              (|_Cover34501| |instance-of| |Cover|)
              (|_Create34500| |instance-of| |Create|)
              (|_Room34496| |instance-of| |Room|)
              (|_Ceiling34495| |instance-of| |Ceiling|)
              (|_Angle-Value34498| |instance-of| |Angle-Value|)
              (|_Be-Stable34503| |actions| |_Be-Stable34503|)
              (|_Be-Stable34503| |primitive-actions| |_Be-Stable34503|)
              (|_Be-Stable34503| |time-during| |_Time-Interval34518|)
              (|_Be-Supported34502| |actions| |_Be-Supported34502|)
              (|_Be-Supported34502| |primitive-actions|
               |_Be-Supported34502|)
              (|_Be-Supported34502| |time-during|
               |_Time-Interval34515|)
              (|_Create34500| |actions| |_Create34500|)
              (|_Create34500| |primitive-actions| |_Create34500|)
              (|_Create34500| |time-during| |_Time-Interval34512|)
              (|_Angle-Value34498| |scalar-value| |_Scalar34499|)
              (|_Ceiling34495| |is-above| |_Room34496|)
              (|_Ceiling34495| |object-of| |_Be-Stable34503|)
              (|_Ceiling34495| |object-of| |_Be-Supported34502|)
              (|_Ceiling34495| |plays| |_Cover34501|)
              (|_Ceiling34495| |result-of| |_Create34500|)
              (|_Ceiling34495| |is-part-of| |_Room34496|)
              (|_Ceiling34495| |orientation| |_Angle-Value34498|))
             T NIL)))
(|Count-Viewpoint| (|_Count-Viewpoint34934|
                    (((|_Count-Viewpoint34934| |instance-of|
                       |Count-Viewpoint|))
                     T NIL)))
(|Unit-of-Measurement| (|_Unit-of-Measurement37422|
                        (((|_Unit-of-Measurement37422| |instance-of|
                           |Unit-of-Measurement|))
                         T NIL)))
(|Wet| (|_Wet24664|
        (((|_Time-Interval24675| |instance-of| |Time-Interval|)
          (|_Tangible-Entity24674| |instance-of| |Tangible-Entity|)
          (|_Wetness-Value24670| |instance-of| |Wetness-Value|)
          (|_Wet24664| |instance-of| |Wet|)
          (|_Wetness-Value24671| |instance-of| |Wetness-Value|)
          (|_Wetness-Value24671| |greater-than| |_Wetness-Value24670|)
          (|_Wet24664| |actions| |_Wet24664|)
          (|_Wet24664| |primitive-actions| |_Wet24664|)
          (|_Wet24664| |time-during| |_Time-Interval24675|)
          (|_Wet24664| |base| |_Tangible-Entity24674|)
          (|_Wet24664| |from-value| |_Wetness-Value24670|)
          (|_Wet24664| |to-value| |_Wetness-Value24671|))
         T NIL)))
(|Thickness-Constant| (|_Thickness-Constant37817|
                       (((|_Thickness-Constant37817| |instance-of|
                          |Thickness-Constant|))
                        T NIL)))
(|UoM-Power| (|_UoM-Power37446|
              (((|_UoM-Power37446| |instance-of| |UoM-Power|)) T NIL)))
(|Be-Shut-Out| (|_Be-Shut-Out271|
                (((|_Container278| |instance-of| |Container|)
                  (|_Time-Interval277| |instance-of| |Time-Interval|)
                  (|_Tangible-Entity275| |instance-of|
                   |Tangible-Entity|)
                  (|_Be-Shut-Out271| |instance-of| |Be-Shut-Out|)
                  (|_Tangible-Entity276| |instance-of|
                   |Tangible-Entity|)
                  (|_Tangible-Entity275| |plays| |_Container278|)
                  (|_Tangible-Entity276| |is-outside|
                   |_Tangible-Entity275|)
                  (|_Be-Shut-Out271| |actions| |_Be-Shut-Out271|)
                  (|_Be-Shut-Out271| |primitive-actions|
                   |_Be-Shut-Out271|)
                  (|_Be-Shut-Out271| |time-during| |_Time-Interval277|)
                  (|_Be-Shut-Out271| |base| |_Tangible-Entity275|)
                  (|_Be-Shut-Out271| |in-event-of|
                   |_Tangible-Entity275|)
                  (|_Be-Shut-Out271| |object| |_Tangible-Entity276|))
                 T NIL)))
(|Sum| (|_Sum34966| (((|_Sum34966| |instance-of| |Sum|)) T NIL)))
(|Person| (|_Person34019|
           (((|_Categorical34021| |instance-of| |Categorical|)
             (|_Agent-Role34022| |instance-of| |Agent-Role|)
             (|_Person34019| |instance-of| |Person|)
             (|_Sentience-Value34020| |instance-of| |Sentience-Value|)
             (|_Sentience-Value34020| |categorical-value|
              |_Categorical34021|)
             (|_Person34019| |capability| |_Agent-Role34022|)
             (|_Person34019| |sentience| |_Sentience-Value34020|))
            T NIL)))
(|Runner| (|_Runner33573|
           (((|_Locomotion33576| |instance-of| |Locomotion|)
             (|_Runner33573| |instance-of| |Runner|)
             (|_Person33575| |instance-of| |Person|)
             (|_Runner33573| |in-event| |_Locomotion33576|)
             (|_Runner33573| |played-by| |_Person33575|))
            T NIL)))
(|Store| (|_Store1914|
          (((|_Container2059| |instance-of| |Container|)
            (|_Time-Interval2001| |instance-of| |Time-Interval|)
            (|_Spatial-Entity1997| |instance-of| |Tangible-Entity|)
            (|_Move1995| |instance-of| |Move|)
            (|_Unblock1993| |instance-of| |Unblock|)
            (|_Time-Interval1992| |instance-of| |Time-Interval|)
            (|_Be-Blocked1991| |instance-of| |Be-Blocked|)
            (|_Move1966| |instance-of| |Move|)
            (|_Block1964| |instance-of| |Block|)
            (|_Time-Interval1963| |instance-of| |Time-Interval|)
            (|_Time-Interval1956| |instance-of| |Time-Interval|)
            (|_Spatial-Entity1949| |instance-of| |Tangible-Entity|)
            (|_Move1946| |instance-of| |Move|)
            (|_Unblock1944| |instance-of| |Unblock|)
            (|_Time-Interval1943| |instance-of| |Time-Interval|)
            (|_Spatial-Entity1936| |instance-of| |Spatial-Entity|)
            (|_Spatial-Entity1935| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity1942| |instance-of| |Tangible-Entity|)
            (|_Be-Blocked1941| |instance-of| |Be-Blocked|)
            (|_Be-Contained1934| |instance-of| |Be-Contained|)
            (|_Be-Contained1934| |instance-of| |Block|)
            (|_Container1933| |instance-of| |Container|)
            (|_Time-Interval1932| |instance-of| |Time-Interval|)
            (|_Tangible-Entity1931| |instance-of| |Tangible-Entity|)
            (|_Entity1927| |instance-of| |Tangible-Entity|)
            (|_Move1924| |instance-of| |Move|)
            (|_Withdraw1923| |instance-of| |Withdraw|)
            (|_Time-Interval1922| |instance-of| |Time-Interval|)
            (|_Tangible-Entity1921| |instance-of| |Tangible-Entity|)
            (|_Entity1918| |instance-of| |Tangible-Entity|)
            (|_Store1914| |instance-of| |Store|)
            (|_Be-Stored1920| |instance-of| |Be-Stored|)
            (|_Tangible-Entity1921| |plays| |_Container2059|)
            (|_Be-Blocked1991| |actions| |_Be-Blocked1991|)
            (|_Be-Blocked1991| |primitive-actions| |_Be-Blocked1991|)
            (|_Be-Blocked1991| |time-during| |_Time-Interval2001|)
            (|_Be-Blocked1991| |object| |_Spatial-Entity1997|)
            (|_Block1964| |actions| |_Block1964|)
            (|_Block1964| |preparatory-event| |_Move1995|)
            (|_Block1964| |preparatory-event| |_Unblock1993|)
            (|_Block1964| |primitive-actions| |_Block1964|)
            (|_Block1964| |time-during| |_Time-Interval1992|)
            (|_Block1964| |base| |_Tangible-Entity1942|)
            (|_Block1964| |object| |_Entity1927|)
            (|_Block1964| |resulting-state| |_Be-Blocked1991|)
            (|_Be-Blocked1941| |object| |_Entity1927|)
            (|_Unblock1944| |actions| |_Unblock1944|)
            (|_Unblock1944| |preparatory-event| |_Move1966|)
            (|_Unblock1944| |preparatory-event| |_Block1964|)
            (|_Unblock1944| |primitive-actions| |_Unblock1944|)
            (|_Unblock1944| |time-during| |_Time-Interval1963|)
            (|_Unblock1944| |base| |_Tangible-Entity1942|)
            (|_Unblock1944| |object| |_Entity1927|)
            (|_Unblock1944| |defeats| |_Be-Blocked1941|)
            (|_Be-Blocked1941| |actions| |_Be-Blocked1941|)
            (|_Be-Blocked1941| |primitive-actions| |_Be-Blocked1941|)
            (|_Be-Blocked1941| |time-during| |_Time-Interval1956|)
            (|_Be-Blocked1941| |object| |_Spatial-Entity1949|)
            (|_Be-Contained1934| |actions| |_Be-Contained1934|)
            (|_Be-Contained1934| |preparatory-event| |_Move1946|)
            (|_Be-Contained1934| |preparatory-event| |_Unblock1944|)
            (|_Be-Contained1934| |primitive-actions|
             |_Be-Contained1934|)
            (|_Be-Contained1934| |time-during| |_Time-Interval1943|)
            (|_Be-Contained1934| |destination| |_Spatial-Entity1936|)
            (|_Be-Contained1934| |origin| |_Spatial-Entity1935|)
            (|_Be-Contained1934| |base| |_Tangible-Entity1942|)
            (|_Be-Contained1934| |object| |_Entity1927|)
            (|_Be-Contained1934| |resulting-state| |_Be-Blocked1941|)
            (|_Be-Contained1934| |instrument| |_Tangible-Entity1931|)
            (|_Entity1927| |is-inside| |_Tangible-Entity1931|)
            (|_Container1933| |in-event| |_Be-Contained1934|)
            (|_Container1933| |content| |_Entity1927|)
            (|_Tangible-Entity1931| |plays| |_Container1933|)
            (|_Be-Stored1920| |actions| |_Be-Stored1920|)
            (|_Be-Stored1920| |primitive-actions| |_Be-Stored1920|)
            (|_Be-Stored1920| |time-during| |_Time-Interval1932|)
            (|_Be-Stored1920| |base| |_Tangible-Entity1931|)
            (|_Be-Stored1920| |object| |_Entity1927|)
            (|_Store1914| |actions| |_Store1914|)
            (|_Store1914| |preparatory-event| |_Move1924|)
            (|_Store1914| |preparatory-event| |_Withdraw1923|)
            (|_Store1914| |primitive-actions| |_Store1914|)
            (|_Store1914| |time-during| |_Time-Interval1922|)
            (|_Store1914| |base| |_Tangible-Entity1921|)
            (|_Store1914| |object| |_Entity1918|)
            (|_Store1914| |resulting-state| |_Be-Stored1920|))
           T NIL)))
(|Specify| (|_Specify2112|
            (((|_Move2121| |instance-of| |Move|)
              (|_Time-Interval2120| |instance-of| |Time-Interval|)
              (|_Entity2117| |instance-of| |Spatial-Entity|)
              (|_Specify2112| |instance-of| |Specify|)
              (|_Specification2119| |instance-of| |Specification|)
              (|_Specify2112| |actions| |_Specify2112|)
              (|_Specify2112| |preparatory-event| |_Move2121|)
              (|_Specify2112| |primitive-actions| |_Specify2112|)
              (|_Specify2112| |time-during| |_Time-Interval2120|)
              (|_Specify2112| |object| |_Entity2117|)
              (|_Specify2112| |result| |_Specification2119|))
             T NIL)))
(|Healing| (|_Healing830|
            (((|_Healing830| |instance-of| |Healing|)
              (|_Time-Interval836| |instance-of| |Time-Interval|)
              (|_Healing830| |actions| |_Healing830|)
              (|_Healing830| |primitive-actions| |_Healing830|)
              (|_Healing830| |time-during| |_Time-Interval836|))
             T NIL)))
(|Country| (|_Country34685|
            (((|_Country34685| |instance-of| |Country|)) T NIL)))
(|SHAKEN-Table| (|_SHAKEN-Table37985|
                 (((|_SHAKEN-Table37985| |instance-of| |SHAKEN-Table|))
                  T NIL)))
(|Solid-Substance| (|_Solid-Substance33607|
                    (((|_Categorical33612| |instance-of| |Categorical|)
                      (|_Solid-Substance33607| |instance-of|
                       |Solid-Substance|)
                      (|_State-Value33611| |instance-of| |State-Value|)
                      (|_State-Value33611| |categorical-value|
                       |_Categorical33612|)
                      (|_Solid-Substance33607| |physical-state|
                       |_State-Value33611|))
                     T NIL)))
(|Dim| (|_Dim25399|
        (((|_Time-Interval25410| |instance-of| |Time-Interval|)
          (|_Tangible-Entity25409| |instance-of| |Tangible-Entity|)
          (|_Brightness-Value25405| |instance-of| |Brightness-Value|)
          (|_Dim25399| |instance-of| |Dim|)
          (|_Brightness-Value25406| |instance-of| |Brightness-Value|)
          (|_Brightness-Value25406| |less-than|
           |_Brightness-Value25405|)
          (|_Dim25399| |actions| |_Dim25399|)
          (|_Dim25399| |primitive-actions| |_Dim25399|)
          (|_Dim25399| |time-during| |_Time-Interval25410|)
          (|_Dim25399| |base| |_Tangible-Entity25409|)
          (|_Dim25399| |from-value| |_Brightness-Value25405|)
          (|_Dim25399| |to-value| |_Brightness-Value25406|))
         T NIL)))
(|Natural-Language| (|_Natural-Language34736|
                     (((|_Natural-Language34736| |instance-of|
                        |Natural-Language|))
                      T NIL)))
(|View| (|_View34817|
         (((|_View34817| |instance-of| |View|)
           (|_Number34819| |instance-of| |Number|)
           (|_View34817| |number-of-elements| |_Number34819|))
          T NIL)))
(|Ray| (|_Ray34702|
        (((|_Text-Field34705| |instance-of| |Text-Field|)
          (|_Ray34702| |instance-of| |Ray|)
          (|_Point34704| |instance-of| |Point|)
          (|_Point34704| |identifier| |_Text-Field34705|)
          (|_Ray34702| |has-part| |_Point34704|))
         T NIL)))
(|Action| (|_Action974|
           (((|_Action974| |instance-of| |Action|)
             (|_Time-Interval980| |instance-of| |Time-Interval|)
             (|_Action974| |actions| |_Action974|)
             (|_Action974| |primitive-actions| |_Action974|)
             (|_Action974| |time-during| |_Time-Interval980|))
            T NIL)))
(|Emit| (|_Emit23183|
         (((|_Move23227| |instance-of| |Move|)
           (|_Move23226| |instance-of| |Move|)
           (|_Time-Interval23225| |instance-of| |Time-Interval|)
           (|_Spatial-Entity23198| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity23210| |instance-of| |Tangible-Entity|)
           (|_Acceleration-Magnitude-Value23201| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value23200| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value23207| |instance-of| |Length-Value|)
           (|_Duration-Value23199| |instance-of| |Duration-Value|)
           (|_Speed-Value23203| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value23206| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value23224| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23221| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23223| |instance-of| |Speed-Value|)
           (|_Speed-Value23222| |instance-of| |Speed-Value|)
           (|_Speed-Value23220| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23217| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value23219| |instance-of| |Speed-Value|)
           (|_Speed-Value23218| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value23202| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value23216| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23215| |instance-of| |Length-Value|)
           (|_Speed-Value23214| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value23213| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value23212| |instance-of| |Length-Value|)
           (|_Emit23183| |instance-of| |Emit|)
           (|_Speed-Value23211| |instance-of| |Speed-Value|)
           (|_Emit23183| |actions| |_Emit23183|)
           (|_Emit23183| |preparatory-event| |_Move23227|)
           (|_Emit23183| |preparatory-event| |_Move23226|)
           (|_Emit23183| |primitive-actions| |_Emit23183|)
           (|_Emit23183| |time-during| |_Time-Interval23225|)
           (|_Emit23183| |origin| |_Spatial-Entity23198|)
           (|_Emit23183| |object| |_Tangible-Entity23210|)
           (|_Emit23183| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value23201|)
           (|_Emit23183| |acceleration|
            |_Acceleration-Vector-Value23200|)
           (|_Emit23183| |distance| |_Length-Value23207|)
           (|_Emit23183| |duration| |_Duration-Value23199|)
           (|_Emit23183| |speed| |_Speed-Value23203|)
           (|_Emit23183| |displacement|
            |_Displacement-Vector-Value23206|)
           (|_Emit23183| |final-speed| |_Speed-Value23224|)
           (|_Emit23183| |final-velocity|
            |_Velocity-Vector-Value23221|)
           (|_Emit23183| |final-x-speed| |_Speed-Value23223|)
           (|_Emit23183| |final-y-speed| |_Speed-Value23222|)
           (|_Emit23183| |initial-speed| |_Speed-Value23220|)
           (|_Emit23183| |initial-velocity|
            |_Velocity-Vector-Value23217|)
           (|_Emit23183| |initial-x-speed| |_Speed-Value23219|)
           (|_Emit23183| |initial-y-speed| |_Speed-Value23218|)
           (|_Emit23183| |velocity| |_Velocity-Vector-Value23202|)
           (|_Emit23183| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23216|)
           (|_Emit23183| |x-distance| |_Length-Value23215|)
           (|_Emit23183| |x-speed| |_Speed-Value23214|)
           (|_Emit23183| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value23213|)
           (|_Emit23183| |y-distance| |_Length-Value23212|)
           (|_Emit23183| |y-speed| |_Speed-Value23211|))
          T NIL)))
(|Angle-Constant| (|_Angle-Constant37919|
                   (((|_Angle-Constant37919| |instance-of|
                      |Angle-Constant|))
                    T NIL)))
(|Be-Damaged| (|_Be-Damaged335|
               (((|_Time-Interval339| |instance-of| |Time-Interval|)
                 (|_Be-Damaged335| |instance-of| |Be-Damaged|)
                 (|_Tangible-Entity338| |instance-of|
                  |Tangible-Entity|)
                 (|_Be-Damaged335| |actions| |_Be-Damaged335|)
                 (|_Be-Damaged335| |primitive-actions|
                  |_Be-Damaged335|)
                 (|_Be-Damaged335| |time-during| |_Time-Interval339|)
                 (|_Be-Damaged335| |object| |_Tangible-Entity338|))
                T NIL)))
(|Goal| (|_Goal410|
         (((|_Goal410| |instance-of| |Goal|)
           (|_Time-Interval412| |instance-of| |Time-Interval|)
           (|_Goal410| |actions| |_Goal410|)
           (|_Goal410| |primitive-actions| |_Goal410|)
           (|_Goal410| |time-during| |_Time-Interval412|))
          T NIL)))
(|Change| (|_Change956|
           (((|_Change956| |instance-of| |Change|)
             (|_Time-Interval962| |instance-of| |Time-Interval|)
             (|_Change956| |actions| |_Change956|)
             (|_Change956| |primitive-actions| |_Change956|)
             (|_Change956| |time-during| |_Time-Interval962|))
            T NIL)))
(|Be-Broken| (|_Be-Broken342|
              (((|_Time-Interval346| |instance-of| |Time-Interval|)
                (|_Be-Broken342| |instance-of| |Be-Broken|)
                (|_Physical-Object345| |instance-of| |Physical-Object|)
                (|_Be-Broken342| |actions| |_Be-Broken342|)
                (|_Be-Broken342| |primitive-actions| |_Be-Broken342|)
                (|_Be-Broken342| |time-during| |_Time-Interval346|)
                (|_Be-Broken342| |object| |_Physical-Object345|))
               T NIL)))
(|Open| (|_Open24485|
         (((|_Portal24499| |instance-of| |Portal|)
           (|_Time-Interval24503| |instance-of| |Time-Interval|)
           (|_Spatial-Entity24498| |instance-of| |Spatial-Entity|)
           (|_Move24494| |instance-of| |Move|)
           (|_Close24491| |instance-of| |Close|)
           (|_Time-Interval24490| |instance-of| |Time-Interval|)
           (|_Spatial-Entity24486| |instance-of| |Spatial-Entity|)
           (|_Be-Closed24489| |instance-of| |Be-Closed|)
           (|_Open24485| |instance-of| |Open|)
           (|_Be-Open24488| |instance-of| |Be-Open|)
           (|_Spatial-Entity24498| |plays| |_Portal24499|)
           (|_Be-Open24488| |actions| |_Be-Open24488|)
           (|_Be-Open24488| |primitive-actions| |_Be-Open24488|)
           (|_Be-Open24488| |time-during| |_Time-Interval24503|)
           (|_Be-Open24488| |object| |_Spatial-Entity24498|)
           (|_Open24485| |actions| |_Open24485|)
           (|_Open24485| |preparatory-event| |_Move24494|)
           (|_Open24485| |preparatory-event| |_Close24491|)
           (|_Open24485| |primitive-actions| |_Open24485|)
           (|_Open24485| |time-during| |_Time-Interval24490|)
           (|_Open24485| |object| |_Spatial-Entity24486|)
           (|_Open24485| |defeats| |_Be-Closed24489|)
           (|_Open24485| |resulting-state| |_Be-Open24488|))
          T NIL)))
(|Sentience-Constant| (|_Sentience-Constant37837|
                       (((|_Sentience-Constant37837| |instance-of|
                          |Sentience-Constant|))
                        T NIL)))
(|Geometric-Object| (|_Geometric-Object34689|
                     (((|_Geometric-Object34689| |instance-of|
                        |Geometric-Object|))
                      T NIL)))
(|Area-Value| (|_Area-Value37756|
               (((|_Area-Value37756| |instance-of| |Area-Value|)) T
                NIL)))
(|Corporation| (|_Corporation34847|
                (((|_Corporation34847| |instance-of| |Corporation|)
                  (|_Number34849| |instance-of| |Number|)
                  (|_Corporation34847| |number-of-elements|
                   |_Number34849|))
                 T NIL)))
(|Shut-Out| (|_Shut-Out23710|
             (((|_Container23729| |instance-of| |Container|)
               (|_Container23726| |instance-of| |Container|)
               (|_Time-Interval23725| |instance-of| |Time-Interval|)
               (|_Tangible-Entity23719| |instance-of|
                |Tangible-Entity|)
               (|_Tangible-Entity23721| |instance-of|
                |Tangible-Entity|)
               (|_Move23718| |instance-of| |Move|)
               (|_Admit23716| |instance-of| |Admit|)
               (|_Time-Interval23715| |instance-of| |Time-Interval|)
               (|_Tangible-Entity23714| |instance-of|
                |Tangible-Entity|)
               (|_Tangible-Entity23711| |instance-of|
                |Tangible-Entity|)
               (|_Shut-Out23710| |instance-of| |Shut-Out|)
               (|_Be-Shut-Out23713| |instance-of| |Be-Shut-Out|)
               (|_Tangible-Entity23714| |plays| |_Container23729|)
               (|_Tangible-Entity23719| |plays| |_Container23726|)
               (|_Tangible-Entity23721| |is-outside|
                |_Tangible-Entity23719|)
               (|_Be-Shut-Out23713| |actions| |_Be-Shut-Out23713|)
               (|_Be-Shut-Out23713| |primitive-actions|
                |_Be-Shut-Out23713|)
               (|_Be-Shut-Out23713| |time-during|
                |_Time-Interval23725|)
               (|_Be-Shut-Out23713| |base| |_Tangible-Entity23719|)
               (|_Be-Shut-Out23713| |in-event-of|
                |_Tangible-Entity23719|)
               (|_Be-Shut-Out23713| |object| |_Tangible-Entity23721|)
               (|_Shut-Out23710| |actions| |_Shut-Out23710|)
               (|_Shut-Out23710| |preparatory-event| |_Move23718|)
               (|_Shut-Out23710| |preparatory-event| |_Admit23716|)
               (|_Shut-Out23710| |primitive-actions| |_Shut-Out23710|)
               (|_Shut-Out23710| |time-during| |_Time-Interval23715|)
               (|_Shut-Out23710| |base| |_Tangible-Entity23714|)
               (|_Shut-Out23710| |object| |_Tangible-Entity23711|)
               (|_Shut-Out23710| |resulting-state|
                |_Be-Shut-Out23713|))
              T NIL)))
(|SHAKEN-Table-Column| (|_SHAKEN-Table-Column33594|
                        (((|_SHAKEN-Table-Column33594| |instance-of|
                           |SHAKEN-Table-Column|))
                         T NIL)))
(|Integrity-Constant| (|_Integrity-Constant37879|
                       (((|_Integrity-Constant37879| |instance-of|
                          |Integrity-Constant|))
                        T NIL)))
(|State-Constant| (|_State-Constant37825|
                   (((|_State-Constant37825| |instance-of|
                      |State-Constant|))
                    T NIL)))
(|Development| (|_Development893|
                (((|_Development893| |instance-of| |Development|)
                  (|_Time-Interval899| |instance-of| |Time-Interval|)
                  (|_Development893| |actions| |_Development893|)
                  (|_Development893| |primitive-actions|
                   |_Development893|)
                  (|_Development893| |time-during|
                   |_Time-Interval899|))
                 T NIL)))
(|Engineering| (|_Engineering902|
                (((|_Engineering902| |instance-of| |Engineering|)
                  (|_Time-Interval908| |instance-of| |Time-Interval|)
                  (|_Engineering902| |actions| |_Engineering902|)
                  (|_Engineering902| |primitive-actions|
                   |_Engineering902|)
                  (|_Engineering902| |time-during|
                   |_Time-Interval908|))
                 T NIL)))
(|Liquid-Substance| (|_Liquid-Substance33859|
                     (((|_Categorical33864| |instance-of|
                        |Categorical|)
                       (|_Liquid-Substance33859| |instance-of|
                        |Liquid-Substance|)
                       (|_State-Value33863| |instance-of|
                        |State-Value|)
                       (|_State-Value33863| |categorical-value|
                        |_Categorical33864|)
                       (|_Liquid-Substance33859| |physical-state|
                        |_State-Value33863|))
                      T NIL)))
(|Message-Field| (|_Message-Field34762|
                  (((|_Language34765| |instance-of| |Language|)
                    (|_Message-Field34762| |instance-of|
                     |Message-Field|)
                    (|_Thing34764| |instance-of| |Thing|)
                    (|_Message-Field34762| |information-language|
                     |_Language34765|)
                    (|_Message-Field34762| |information-content|
                     |_Thing34764|))
                   T NIL)))
(|Quadrilateral| (|_Quadrilateral34714|
                  (((|_Quadrilateral34714| |instance-of|
                     |Quadrilateral|))
                   T NIL)))
(|County| (|_County34683|
           (((|_County34683| |instance-of| |County|)) T NIL)))
(|Duration-Scale| (|_Duration-Scale37597|
                   (((|_Duration-Scale37597| |instance-of|
                      |Duration-Scale|)
                     (|_Number37599| |instance-of| |Number|)
                     (|_Duration-Scale37597| |number-of-elements|
                      |_Number37599|))
                    T NIL)))
(|Baseball| (|_Baseball34596|
             (((|_Baseball34596| |instance-of| |Baseball|)) T NIL)))
(|Throw| (|_Throw10455|
          (((|_Time-Interval10624| |instance-of| |Time-Interval|)
            (|_Entity10620| |instance-of| |Entity|)
            (|_Move10618| |instance-of| |Move|)
            (|_Make-Accessible10617| |instance-of| |Make-Accessible|)
            (|_Time-Interval10616| |instance-of| |Time-Interval|)
            (|_Be-Inaccessible10615| |instance-of| |Be-Inaccessible|)
            (|_Move10572| |instance-of| |Move|)
            (|_Make-Inaccessible10571| |instance-of|
             |Make-Inaccessible|)
            (|_Time-Interval10570| |instance-of| |Time-Interval|)
            (|_Fall10569| |instance-of| |Fall|)
            (|_Tangible-Entity10563| |instance-of| |Tangible-Entity|)
            (|_Move10497| |instance-of| |Move|)
            (|_Time-Interval10496| |instance-of| |Time-Interval|)
            (|_Tangible-Entity10495| |instance-of| |Tangible-Entity|)
            (|_Let-Go-Of10494| |instance-of| |Let-Go-Of|)
            (|_Propel10493| |instance-of| |Propel|)
            (|_Acceleration-Magnitude-Value10470| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value10469| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value10476| |instance-of| |Length-Value|)
            (|_Duration-Value10468| |instance-of| |Duration-Value|)
            (|_Speed-Value10472| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value10475| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value10492| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value10489| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value10491| |instance-of| |Speed-Value|)
            (|_Speed-Value10490| |instance-of| |Speed-Value|)
            (|_Speed-Value10488| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value10485| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value10487| |instance-of| |Speed-Value|)
            (|_Speed-Value10486| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value10471| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value10484| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value10483| |instance-of| |Length-Value|)
            (|_Speed-Value10482| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value10481| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value10480| |instance-of| |Length-Value|)
            (|_Throw10455| |instance-of| |Throw|)
            (|_Speed-Value10479| |instance-of| |Speed-Value|)
            (|_Be-Inaccessible10615| |actions| |_Be-Inaccessible10615|)
            (|_Be-Inaccessible10615| |primitive-actions|
             |_Be-Inaccessible10615|)
            (|_Be-Inaccessible10615| |time-during|
             |_Time-Interval10624|)
            (|_Be-Inaccessible10615| |object| |_Entity10620|)
            (|_Make-Inaccessible10571| |actions|
             |_Make-Inaccessible10571|)
            (|_Make-Inaccessible10571| |preparatory-event|
             |_Move10618|)
            (|_Make-Inaccessible10571| |preparatory-event|
             |_Make-Accessible10617|)
            (|_Make-Inaccessible10571| |primitive-actions|
             |_Make-Inaccessible10571|)
            (|_Make-Inaccessible10571| |time-during|
             |_Time-Interval10616|)
            (|_Make-Inaccessible10571| |object|
             |_Tangible-Entity10495|)
            (|_Make-Inaccessible10571| |resulting-state|
             |_Be-Inaccessible10615|)
            (|_Make-Inaccessible10571| |agent| |_Tangible-Entity10563|)
            (|_Let-Go-Of10494| |actions-of| |_Let-Go-Of10494|)
            (|_Let-Go-Of10494| |preparatory-event| |_Move10572|)
            (|_Let-Go-Of10494| |preparatory-event|
             |_Make-Inaccessible10571|)
            (|_Let-Go-Of10494| |primitive-actions-of|
             |_Let-Go-Of10494|)
            (|_Let-Go-Of10494| |time-during| |_Time-Interval10570|)
            (|_Let-Go-Of10494| |object| |_Tangible-Entity10495|)
            (|_Let-Go-Of10494| |causes| |_Fall10569|)
            (|_Let-Go-Of10494| |agent| |_Tangible-Entity10563|)
            (|_Throw10455| |actions| |_Throw10455|)
            (|_Throw10455| |actions| |_Let-Go-Of10494|)
            (|_Throw10455| |actions| |_Propel10493|)
            (|_Throw10455| |all-subevents| |_Let-Go-Of10494|)
            (|_Throw10455| |all-subevents| |_Propel10493|)
            (|_Throw10455| |preparatory-event| |_Move10497|)
            (|_Throw10455| |primitive-actions| |_Let-Go-Of10494|)
            (|_Throw10455| |primitive-actions| |_Propel10493|)
            (|_Throw10455| |time-during| |_Time-Interval10496|)
            (|_Throw10455| |object| |_Tangible-Entity10495|)
            (|_Throw10455| |first-subevent| |_Propel10493|)
            (|_Throw10455| |subevent| |_Let-Go-Of10494|)
            (|_Throw10455| |subevent| |_Propel10493|)
            (|_Throw10455| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value10470|)
            (|_Throw10455| |acceleration|
             |_Acceleration-Vector-Value10469|)
            (|_Throw10455| |distance| |_Length-Value10476|)
            (|_Throw10455| |duration| |_Duration-Value10468|)
            (|_Throw10455| |speed| |_Speed-Value10472|)
            (|_Throw10455| |displacement|
             |_Displacement-Vector-Value10475|)
            (|_Throw10455| |final-speed| |_Speed-Value10492|)
            (|_Throw10455| |final-velocity|
             |_Velocity-Vector-Value10489|)
            (|_Throw10455| |final-x-speed| |_Speed-Value10491|)
            (|_Throw10455| |final-y-speed| |_Speed-Value10490|)
            (|_Throw10455| |initial-speed| |_Speed-Value10488|)
            (|_Throw10455| |initial-velocity|
             |_Velocity-Vector-Value10485|)
            (|_Throw10455| |initial-x-speed| |_Speed-Value10487|)
            (|_Throw10455| |initial-y-speed| |_Speed-Value10486|)
            (|_Throw10455| |velocity| |_Velocity-Vector-Value10471|)
            (|_Throw10455| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value10484|)
            (|_Throw10455| |x-distance| |_Length-Value10483|)
            (|_Throw10455| |x-speed| |_Speed-Value10482|)
            (|_Throw10455| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value10481|)
            (|_Throw10455| |y-distance| |_Length-Value10480|)
            (|_Throw10455| |y-speed| |_Speed-Value10479|))
           T NIL)))
(|Line-Segment| (|_Line-Segment34695|
                 (((|_Text-Field34701| |instance-of| |Text-Field|)
                   (|_Text-Field34700| |instance-of| |Text-Field|)
                   (|_Point34698| |instance-of| |Point|)
                   (|_Line-Segment34695| |instance-of| |Line-Segment|)
                   (|_Point34697| |instance-of| |Point|)
                   (|_Point34698| |identifier| |_Text-Field34701|)
                   (|_Point34697| |identifier| |_Text-Field34700|)
                   (|_Line-Segment34695| |has-part| |_Point34698|)
                   (|_Line-Segment34695| |has-part| |_Point34697|))
                  T NIL)))
(|Innovation| (|_Innovation812|
               (((|_Innovation812| |instance-of| |Innovation|)
                 (|_Time-Interval818| |instance-of| |Time-Interval|)
                 (|_Innovation812| |actions| |_Innovation812|)
                 (|_Innovation812| |primitive-actions|
                  |_Innovation812|)
                 (|_Innovation812| |time-during| |_Time-Interval818|))
                T NIL)))
(|Worth-Value| (|_Worth-Value37675|
                (((|_Worth-Value37675| |instance-of| |Worth-Value|)) T
                 NIL)))
(|Luminance-Constant| (|_Luminance-Constant37869|
                       (((|_Luminance-Constant37869| |instance-of|
                          |Luminance-Constant|))
                        T NIL)))
(|Property-Class-Order| (|_Property-Class-Order37790|
                         (((|_Property-Class-Order37790| |instance-of|
                            |Property-Class-Order|)
                           (|_Number37792| |instance-of| |Number|)
                           (|_Property-Class-Order37790|
                            |number-of-elements| |_Number37792|))
                          T NIL)))
(|Cool| (|_Cool25415|
         (((|_Time-Interval25426| |instance-of| |Time-Interval|)
           (|_Tangible-Entity25425| |instance-of| |Tangible-Entity|)
           (|_Temperature-Value25421| |instance-of|
            |Temperature-Value|)
           (|_Cool25415| |instance-of| |Cool|)
           (|_Temperature-Value25422| |instance-of|
            |Temperature-Value|)
           (|_Temperature-Value25422| |less-than|
            |_Temperature-Value25421|)
           (|_Cool25415| |actions| |_Cool25415|)
           (|_Cool25415| |primitive-actions| |_Cool25415|)
           (|_Cool25415| |time-during| |_Time-Interval25426|)
           (|_Cool25415| |base| |_Tangible-Entity25425|)
           (|_Cool25415| |from-value| |_Temperature-Value25421|)
           (|_Cool25415| |to-value| |_Temperature-Value25422|))
          T NIL)))
(|Pupil| (|_Pupil32983|
          (((|_Pupil32983| |instance-of| |Pupil|)
            (|_Tangible-Entity32984| |instance-of| |Tangible-Entity|)
            (|_Pupil32983| |played-by| |_Tangible-Entity32984|))
           T NIL)))
(|Spatial-Relation| (|_Spatial-Relation37943|
                     (((|_Spatial-Relation37943| |instance-of|
                        |Spatial-Relation|))
                      T NIL)))
(|Unrestrain| (|_Unrestrain24189|
               (((|_Move24195| |instance-of| |Move|)
                 (|_Restrain24193| |instance-of| |Restrain|)
                 (|_Time-Interval24192| |instance-of| |Time-Interval|)
                 (|_Unrestrain24189| |instance-of| |Unrestrain|)
                 (|_Tangible-Entity24190| |instance-of|
                  |Tangible-Entity|)
                 (|_Unrestrain24189| |actions| |_Unrestrain24189|)
                 (|_Unrestrain24189| |preparatory-event| |_Move24195|)
                 (|_Unrestrain24189| |preparatory-event|
                  |_Restrain24193|)
                 (|_Unrestrain24189| |primitive-actions|
                  |_Unrestrain24189|)
                 (|_Unrestrain24189| |time-during|
                  |_Time-Interval24192|)
                 (|_Unrestrain24189| |object| |_Tangible-Entity24190|))
                T NIL)))
(|Room| (|_Room34253|
         (((|_Time-Interval34276| |instance-of| |Time-Interval|)
           (|_Time-Interval34273| |instance-of| |Time-Interval|)
           (|_Time-Interval34270| |instance-of| |Time-Interval|)
           (|_Be-Stable34262| |instance-of| |Be-Stable|)
           (|_Be-Supported34261| |instance-of| |Be-Supported|)
           (|_Container34260| |instance-of| |Container|)
           (|_Create34259| |instance-of| |Create|)
           (|_Ceiling34258| |instance-of| |Ceiling|)
           (|_Wall34257| |instance-of| |Wall|)
           (|_Floor34256| |instance-of| |Floor|)
           (|_Room34253| |instance-of| |Room|)
           (|_Building34255| |instance-of| |Building|)
           (|_Be-Stable34262| |actions| |_Be-Stable34262|)
           (|_Be-Stable34262| |primitive-actions| |_Be-Stable34262|)
           (|_Be-Stable34262| |time-during| |_Time-Interval34276|)
           (|_Be-Supported34261| |actions| |_Be-Supported34261|)
           (|_Be-Supported34261| |primitive-actions|
            |_Be-Supported34261|)
           (|_Be-Supported34261| |time-during| |_Time-Interval34273|)
           (|_Create34259| |actions| |_Create34259|)
           (|_Create34259| |primitive-actions| |_Create34259|)
           (|_Create34259| |time-during| |_Time-Interval34270|)
           (|_Room34253| |object-of| |_Be-Stable34262|)
           (|_Room34253| |object-of| |_Be-Supported34261|)
           (|_Room34253| |plays| |_Container34260|)
           (|_Room34253| |result-of| |_Create34259|)
           (|_Room34253| |has-part| |_Ceiling34258|)
           (|_Room34253| |has-part| |_Wall34257|)
           (|_Room34253| |has-part| |_Floor34256|)
           (|_Room34253| |is-part-of| |_Building34255|))
          T NIL)))
(|Capacity-Constant| (|_Capacity-Constant37909|
                      (((|_Capacity-Constant37909| |instance-of|
                         |Capacity-Constant|))
                       T NIL)))
(|Brightness-Scale| (|_Brightness-Scale37637|
                     (((|_Brightness-Scale37637| |instance-of|
                        |Brightness-Scale|)
                       (|_Number37639| |instance-of| |Number|)
                       (|_Brightness-Scale37637| |number-of-elements|
                        |_Number37639|))
                      T NIL)))
(|Atomic-Weight-Value| (|_Atomic-Weight-Value37722|
                        (((|_Atomic-Weight-Value37722| |instance-of|
                           |Atomic-Weight-Value|))
                         T NIL)))
(|Viewpoint| (|_Viewpoint34922|
              (((|_Viewpoint34922| |instance-of| |Viewpoint|)) T NIL)))
(|Command| (|_Command25213|
            (((|_Move25227| |instance-of| |Move|)
              (|_Time-Interval25226| |instance-of| |Time-Interval|)
              (|_Information25223| |instance-of| |Information|)
              (|_Information25223| |instance-of| |Spatial-Entity|)
              (|_Tangible-Entity25221| |instance-of| |Tangible-Entity|)
              (|_Message25224| |instance-of| |Message|)
              (|_Command25213| |instance-of| |Command|)
              (|_Tangible-Entity25220| |instance-of| |Tangible-Entity|)
              (|_Command25213| |actions| |_Command25213|)
              (|_Command25213| |preparatory-event| |_Move25227|)
              (|_Command25213| |primitive-actions| |_Command25213|)
              (|_Command25213| |time-during| |_Time-Interval25226|)
              (|_Command25213| |object| |_Information25223|)
              (|_Command25213| |recipient| |_Tangible-Entity25221|)
              (|_Command25213| |result| |_Message25224|)
              (|_Command25213| |agent| |_Tangible-Entity25220|))
             T NIL)))
(|Property-Group| (|_Property-Group37420|
                   (((|_Property-Group37420| |instance-of|
                      |Property-Group|))
                    T NIL)))
(|Train| (|_Train34633|
          (((|_Train34633| |instance-of| |Train|)) T NIL)))
(|Wetness-Scale| (|_Wetness-Scale37502|
                  (((|_Wetness-Scale37502| |instance-of|
                     |Wetness-Scale|)
                    (|_Number37504| |instance-of| |Number|)
                    (|_Wetness-Scale37502| |number-of-elements|
                     |_Number37504|))
                   T NIL)))
(|Area-Scale| (|_Area-Scale37647|
               (((|_Area-Scale37647| |instance-of| |Area-Scale|)
                 (|_Number37649| |instance-of| |Number|)
                 (|_Area-Scale37647| |number-of-elements|
                  |_Number37649|))
                T NIL)))
(|Floor| (|_Floor34454|
          (((|_Time-Interval34476| |instance-of| |Time-Interval|)
            (|_Time-Interval34473| |instance-of| |Time-Interval|)
            (|_Time-Interval34470| |instance-of| |Time-Interval|)
            (|_Container34462| |instance-of| |Container|)
            (|_Scalar34456| |instance-of| |Scalar|)
            (|_Be-Stable34460| |instance-of| |Be-Stable|)
            (|_Be-Supported34459| |instance-of| |Be-Supported|)
            (|_Create34458| |instance-of| |Create|)
            (|_Tangible-Entity34457| |instance-of| |Tangible-Entity|)
            (|_Floor34454| |instance-of| |Floor|)
            (|_Angle-Value34455| |instance-of| |Angle-Value|)
            (|_Be-Stable34460| |actions| |_Be-Stable34460|)
            (|_Be-Stable34460| |primitive-actions| |_Be-Stable34460|)
            (|_Be-Stable34460| |time-during| |_Time-Interval34476|)
            (|_Be-Supported34459| |actions| |_Be-Supported34459|)
            (|_Be-Supported34459| |primitive-actions|
             |_Be-Supported34459|)
            (|_Be-Supported34459| |time-during| |_Time-Interval34473|)
            (|_Create34458| |actions| |_Create34458|)
            (|_Create34458| |primitive-actions| |_Create34458|)
            (|_Create34458| |time-during| |_Time-Interval34470|)
            (|_Tangible-Entity34457| |plays| |_Container34462|)
            (|_Angle-Value34455| |scalar-value| |_Scalar34456|)
            (|_Floor34454| |object-of| |_Be-Stable34460|)
            (|_Floor34454| |object-of| |_Be-Supported34459|)
            (|_Floor34454| |result-of| |_Create34458|)
            (|_Floor34454| |is-part-of| |_Tangible-Entity34457|)
            (|_Floor34454| |orientation| |_Angle-Value34455|))
           T NIL)))
(|Building| (|_Building34521|
             (((|_Time-Interval34553| |instance-of| |Time-Interval|)
               (|_Time-Interval34550| |instance-of| |Time-Interval|)
               (|_Time-Interval34547| |instance-of| |Time-Interval|)
               (|_Time-Interval34542| |instance-of| |Time-Interval|)
               (|_Time-Interval34539| |instance-of| |Time-Interval|)
               (|_Time-Interval34536| |instance-of| |Time-Interval|)
               (|_Be-Stable34533| |instance-of| |Be-Stable|)
               (|_Be-Supported34532| |instance-of| |Be-Supported|)
               (|_Cover34531| |instance-of| |Cover|)
               (|_Create34530| |instance-of| |Create|)
               (|_Be-Stable34529| |instance-of| |Be-Stable|)
               (|_Be-Supported34528| |instance-of| |Be-Supported|)
               (|_Container34527| |instance-of| |Container|)
               (|_Create34526| |instance-of| |Create|)
               (|_Roof34525| |instance-of| |Roof|)
               (|_Wall34524| |instance-of| |Wall|)
               (|_Floor34523| |instance-of| |Floor|)
               (|_Building34521| |instance-of| |Building|)
               (|_Room34522| |instance-of| |Room|)
               (|_Be-Stable34529| |actions| |_Be-Stable34529|)
               (|_Be-Stable34529| |primitive-actions|
                |_Be-Stable34529|)
               (|_Be-Stable34529| |time-during| |_Time-Interval34553|)
               (|_Be-Supported34528| |actions| |_Be-Supported34528|)
               (|_Be-Supported34528| |primitive-actions|
                |_Be-Supported34528|)
               (|_Be-Supported34528| |time-during|
                |_Time-Interval34550|)
               (|_Create34526| |actions| |_Create34526|)
               (|_Create34526| |primitive-actions| |_Create34526|)
               (|_Create34526| |time-during| |_Time-Interval34547|)
               (|_Be-Stable34533| |actions| |_Be-Stable34533|)
               (|_Be-Stable34533| |primitive-actions|
                |_Be-Stable34533|)
               (|_Be-Stable34533| |time-during| |_Time-Interval34542|)
               (|_Be-Supported34532| |actions| |_Be-Supported34532|)
               (|_Be-Supported34532| |primitive-actions|
                |_Be-Supported34532|)
               (|_Be-Supported34532| |time-during|
                |_Time-Interval34539|)
               (|_Create34530| |actions| |_Create34530|)
               (|_Create34530| |primitive-actions| |_Create34530|)
               (|_Create34530| |time-during| |_Time-Interval34536|)
               (|_Roof34525| |is-above| |_Building34521|)
               (|_Roof34525| |object-of| |_Be-Stable34533|)
               (|_Roof34525| |object-of| |_Be-Supported34532|)
               (|_Roof34525| |plays| |_Cover34531|)
               (|_Roof34525| |result-of| |_Create34530|)
               (|_Building34521| |object-of| |_Be-Stable34529|)
               (|_Building34521| |object-of| |_Be-Supported34528|)
               (|_Building34521| |plays| |_Container34527|)
               (|_Building34521| |result-of| |_Create34526|)
               (|_Building34521| |has-part| |_Roof34525|)
               (|_Building34521| |has-part| |_Wall34524|)
               (|_Building34521| |has-part| |_Floor34523|)
               (|_Building34521| |has-part| |_Room34522|))
              T NIL)))
(|Teach| (|_Teach29361|
          (((|_Move29449| |instance-of| |Move|)
            (|_Time-Interval29448| |instance-of| |Time-Interval|)
            (|_Be-Known29447| |instance-of| |Be-Known|)
            (|_Move29446| |instance-of| |Move|)
            (|_Time-Interval29445| |instance-of| |Time-Interval|)
            (|_Move29443| |instance-of| |Move|)
            (|_Time-Interval29442| |instance-of| |Time-Interval|)
            (|_Move29441| |instance-of| |Move|)
            (|_Time-Interval29440| |instance-of| |Time-Interval|)
            (|_Acceleration-Magnitude-Value29439| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value29423| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value29438| |instance-of| |Length-Value|)
            (|_Duration-Value29437| |instance-of| |Duration-Value|)
            (|_Speed-Value29436| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value29421| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value29435| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value29432| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value29434| |instance-of| |Speed-Value|)
            (|_Speed-Value29433| |instance-of| |Speed-Value|)
            (|_Speed-Value29431| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value29428| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value29430| |instance-of| |Speed-Value|)
            (|_Speed-Value29429| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value29419| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value29427| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value29426| |instance-of| |Length-Value|)
            (|_Speed-Value29425| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value29424| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value29422| |instance-of| |Length-Value|)
            (|_Speed-Value29420| |instance-of| |Speed-Value|)
            (|_Move29418| |instance-of| |Move|)
            (|_Time-Interval29417| |instance-of| |Time-Interval|)
            (|_Tangible-Entity29416| |instance-of| |Tangible-Entity|)
            (|_Move29414| |instance-of| |Move|)
            (|_Time-Interval29413| |instance-of| |Time-Interval|)
            (|_Move29411| |instance-of| |Move|)
            (|_Time-Interval29410| |instance-of| |Time-Interval|)
            (|_Acceleration-Magnitude-Value29409| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value29393| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value29408| |instance-of| |Length-Value|)
            (|_Duration-Value29407| |instance-of| |Duration-Value|)
            (|_Speed-Value29406| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value29391| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value29405| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value29402| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value29404| |instance-of| |Speed-Value|)
            (|_Speed-Value29403| |instance-of| |Speed-Value|)
            (|_Speed-Value29401| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value29398| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value29400| |instance-of| |Speed-Value|)
            (|_Speed-Value29399| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value29389| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value29397| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value29396| |instance-of| |Length-Value|)
            (|_Speed-Value29395| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value29394| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value29392| |instance-of| |Length-Value|)
            (|_Speed-Value29390| |instance-of| |Speed-Value|)
            (|_Language29388| |instance-of| |Language|)
            (|_Thing29387| |instance-of| |Thing|)
            (|_Thing29386| |instance-of| |Thing|)
            (|_Pupil29385| |instance-of| |Pupil|)
            (|_Time-Interval29384| |instance-of| |Time-Interval|)
            (|_Tangible-Entity29383| |instance-of| |Living-Entity|)
            (|_Entity29382| |instance-of| |Entity|)
            (|_Teacher29381| |instance-of| |Teacher|)
            (|_Move29380| |instance-of| |Move|)
            (|_Sense29379| |instance-of| |Sense|)
            (|_Transmit29378| |instance-of| |Transmit|)
            (|_Embody29377| |instance-of| |Embody|)
            (|_Time-Interval29376| |instance-of| |Time-Interval|)
            (|_Message29369| |instance-of| |Message|)
            (|_Information29375| |instance-of| |Information|)
            (|_Information29375| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity29372| |instance-of| |Tangible-Entity|)
            (|_Convey29365| |instance-of| |Convey|)
            (|_Express29364| |instance-of| |Express|)
            (|_Learn29366| |instance-of| |Learn|)
            (|_Be-Known29363| |instance-of| |Be-Known|)
            (|_Teach29361| |instance-of| |Teach|)
            (|_Tangible-Entity29362| |instance-of| |Tangible-Entity|)
            (|_Learn29366| |actions-of| |_Learn29366|)
            (|_Learn29366| |preparatory-event| |_Move29449|)
            (|_Learn29366| |primitive-actions-of| |_Learn29366|)
            (|_Learn29366| |time-during| |_Time-Interval29448|)
            (|_Learn29366| |object| |_Message29369|)
            (|_Learn29366| |result| |_Information29375|)
            (|_Learn29366| |resulting-state| |_Be-Known29447|)
            (|_Learn29366| |agent| |_Tangible-Entity29372|)
            (|_Convey29365| |actions-of| |_Convey29365|)
            (|_Convey29365| |preparatory-event| |_Move29446|)
            (|_Convey29365| |time-during| |_Time-Interval29445|)
            (|_Convey29365| |base| |_Tangible-Entity29416|)
            (|_Convey29365| |object| |_Message29369|)
            (|_Convey29365| |recipient| |_Tangible-Entity29372|)
            (|_Convey29365| |next-event| |_Learn29366|)
            (|_Convey29365| |agent| |_Tangible-Entity29362|)
            (|_Sense29379| |actions-of| |_Convey29365|)
            (|_Sense29379| |actions-of| |_Sense29379|)
            (|_Sense29379| |all-subevents-of| |_Convey29365|)
            (|_Sense29379| |preparatory-event| |_Move29443|)
            (|_Sense29379| |primitive-actions-of| |_Convey29365|)
            (|_Sense29379| |primitive-actions-of| |_Sense29379|)
            (|_Sense29379| |time-during| |_Time-Interval29442|)
            (|_Sense29379| |experiencer| |_Tangible-Entity29372|)
            (|_Sense29379| |object| |_Tangible-Entity29416|)
            (|_Sense29379| |result| |_Message29369|)
            (|_Sense29379| |subevent-of| |_Convey29365|)
            (|_Sense29379| |agent| |_Tangible-Entity29372|)
            (|_Transmit29378| |actions-of| |_Convey29365|)
            (|_Transmit29378| |actions-of| |_Transmit29378|)
            (|_Transmit29378| |all-subevents-of| |_Convey29365|)
            (|_Transmit29378| |preparatory-event| |_Move29441|)
            (|_Transmit29378| |primitive-actions-of| |_Convey29365|)
            (|_Transmit29378| |primitive-actions-of| |_Transmit29378|)
            (|_Transmit29378| |time-during| |_Time-Interval29440|)
            (|_Transmit29378| |destination| |_Tangible-Entity29372|)
            (|_Transmit29378| |object| |_Tangible-Entity29416|)
            (|_Transmit29378| |subevent-of| |_Convey29365|)
            (|_Transmit29378| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value29439|)
            (|_Transmit29378| |acceleration|
             |_Acceleration-Vector-Value29423|)
            (|_Transmit29378| |distance| |_Length-Value29438|)
            (|_Transmit29378| |duration| |_Duration-Value29437|)
            (|_Transmit29378| |speed| |_Speed-Value29436|)
            (|_Transmit29378| |displacement|
             |_Displacement-Vector-Value29421|)
            (|_Transmit29378| |final-speed| |_Speed-Value29435|)
            (|_Transmit29378| |final-velocity|
             |_Velocity-Vector-Value29432|)
            (|_Transmit29378| |final-x-speed| |_Speed-Value29434|)
            (|_Transmit29378| |final-y-speed| |_Speed-Value29433|)
            (|_Transmit29378| |initial-speed| |_Speed-Value29431|)
            (|_Transmit29378| |initial-velocity|
             |_Velocity-Vector-Value29428|)
            (|_Transmit29378| |initial-x-speed| |_Speed-Value29430|)
            (|_Transmit29378| |initial-y-speed| |_Speed-Value29429|)
            (|_Transmit29378| |velocity| |_Velocity-Vector-Value29419|)
            (|_Transmit29378| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value29427|)
            (|_Transmit29378| |x-distance| |_Length-Value29426|)
            (|_Transmit29378| |x-speed| |_Speed-Value29425|)
            (|_Transmit29378| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value29424|)
            (|_Transmit29378| |y-distance| |_Length-Value29422|)
            (|_Transmit29378| |y-speed| |_Speed-Value29420|)
            (|_Embody29377| |actions-of| |_Convey29365|)
            (|_Embody29377| |actions-of| |_Embody29377|)
            (|_Embody29377| |all-subevents-of| |_Convey29365|)
            (|_Embody29377| |preparatory-event| |_Move29418|)
            (|_Embody29377| |primitive-actions-of| |_Convey29365|)
            (|_Embody29377| |primitive-actions-of| |_Embody29377|)
            (|_Embody29377| |time-during| |_Time-Interval29417|)
            (|_Embody29377| |object| |_Message29369|)
            (|_Embody29377| |result| |_Tangible-Entity29416|)
            (|_Embody29377| |first-subevent-of| |_Convey29365|)
            (|_Embody29377| |next-event| |_Transmit29378|)
            (|_Embody29377| |subevent-of| |_Convey29365|)
            (|_Embody29377| |agent| |_Tangible-Entity29362|)
            (|_Express29364| |actions-of| |_Express29364|)
            (|_Express29364| |preparatory-event| |_Move29414|)
            (|_Express29364| |primitive-actions-of| |_Express29364|)
            (|_Express29364| |time-during| |_Time-Interval29413|)
            (|_Express29364| |object| |_Information29375|)
            (|_Express29364| |result| |_Message29369|)
            (|_Express29364| |next-event| |_Convey29365|)
            (|_Express29364| |agent| |_Tangible-Entity29362|)
            (|_Move29380| |actions| |_Move29380|)
            (|_Move29380| |preparatory-event| |_Move29411|)
            (|_Move29380| |primitive-actions| |_Move29380|)
            (|_Move29380| |time-during| |_Time-Interval29410|)
            (|_Move29380| |destination| |_Information29375|)
            (|_Move29380| |object| |_Tangible-Entity29362|)
            (|_Move29380| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value29409|)
            (|_Move29380| |acceleration|
             |_Acceleration-Vector-Value29393|)
            (|_Move29380| |distance| |_Length-Value29408|)
            (|_Move29380| |duration| |_Duration-Value29407|)
            (|_Move29380| |speed| |_Speed-Value29406|)
            (|_Move29380| |displacement|
             |_Displacement-Vector-Value29391|)
            (|_Move29380| |final-speed| |_Speed-Value29405|)
            (|_Move29380| |final-velocity|
             |_Velocity-Vector-Value29402|)
            (|_Move29380| |final-x-speed| |_Speed-Value29404|)
            (|_Move29380| |final-y-speed| |_Speed-Value29403|)
            (|_Move29380| |initial-speed| |_Speed-Value29401|)
            (|_Move29380| |initial-velocity|
             |_Velocity-Vector-Value29398|)
            (|_Move29380| |initial-x-speed| |_Speed-Value29400|)
            (|_Move29380| |initial-y-speed| |_Speed-Value29399|)
            (|_Move29380| |velocity| |_Velocity-Vector-Value29389|)
            (|_Move29380| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value29397|)
            (|_Move29380| |x-distance| |_Length-Value29396|)
            (|_Move29380| |x-speed| |_Speed-Value29395|)
            (|_Move29380| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value29394|)
            (|_Move29380| |y-distance| |_Length-Value29392|)
            (|_Move29380| |y-speed| |_Speed-Value29390|)
            (|_Message29369| |information-language| |_Language29388|)
            (|_Message29369| |information-content| |_Thing29387|)
            (|_Information29375| |information-content| |_Thing29386|)
            (|_Tangible-Entity29372| |plays| |_Pupil29385|)
            (|_Be-Known29363| |actions| |_Be-Known29363|)
            (|_Be-Known29363| |primitive-actions| |_Be-Known29363|)
            (|_Be-Known29363| |time-during| |_Time-Interval29384|)
            (|_Be-Known29363| |experiencer| |_Tangible-Entity29383|)
            (|_Be-Known29363| |object| |_Entity29382|)
            (|_Tangible-Entity29362| |plays| |_Teacher29381|)
            (|_Teach29361| |actions| |_Teach29361|)
            (|_Teach29361| |actions| |_Learn29366|)
            (|_Teach29361| |actions| |_Convey29365|)
            (|_Teach29361| |actions| |_Sense29379|)
            (|_Teach29361| |actions| |_Transmit29378|)
            (|_Teach29361| |actions| |_Embody29377|)
            (|_Teach29361| |actions| |_Express29364|)
            (|_Teach29361| |all-subevents| |_Sense29379|)
            (|_Teach29361| |all-subevents| |_Transmit29378|)
            (|_Teach29361| |all-subevents| |_Embody29377|)
            (|_Teach29361| |all-subevents| |_Learn29366|)
            (|_Teach29361| |all-subevents| |_Convey29365|)
            (|_Teach29361| |all-subevents| |_Express29364|)
            (|_Teach29361| |preparatory-event| |_Move29380|)
            (|_Teach29361| |primitive-actions| |_Learn29366|)
            (|_Teach29361| |primitive-actions| |_Sense29379|)
            (|_Teach29361| |primitive-actions| |_Transmit29378|)
            (|_Teach29361| |primitive-actions| |_Embody29377|)
            (|_Teach29361| |primitive-actions| |_Express29364|)
            (|_Teach29361| |time-during| |_Time-Interval29376|)
            (|_Teach29361| |base| |_Message29369|)
            (|_Teach29361| |object| |_Information29375|)
            (|_Teach29361| |recipient| |_Tangible-Entity29372|)
            (|_Teach29361| |first-subevent| |_Express29364|)
            (|_Teach29361| |subevent| |_Learn29366|)
            (|_Teach29361| |subevent| |_Convey29365|)
            (|_Teach29361| |subevent| |_Express29364|)
            (|_Teach29361| |objective| |_Learn29366|)
            (|_Teach29361| |resulting-state| |_Be-Known29363|)
            (|_Teach29361| |agent| |_Tangible-Entity29362|)
            (|_Teach29361| |donor| |_Tangible-Entity29362|))
           NIL NIL)))
(|Luminous-Intensity-Constant| (|_Luminous-Intensity-Constant37865|
                                (((|_Luminous-Intensity-Constant37865|
                                   |instance-of|
                                   |Luminous-Intensity-Constant|))
                                 T
                                 NIL)))
(|Diameter-Constant| (|_Diameter-Constant37895|
                      (((|_Diameter-Constant37895| |instance-of|
                         |Diameter-Constant|))
                       T NIL)))
(|Perceive| (|_Perceive10044|
             (((|_Move10051| |instance-of| |Move|)
               (|_Time-Interval10050| |instance-of| |Time-Interval|)
               (|_Perceive10044| |instance-of| |Perceive|)
               (|_Entity10048| |instance-of| |Spatial-Entity|)
               (|_Perceive10044| |actions| |_Perceive10044|)
               (|_Perceive10044| |preparatory-event| |_Move10051|)
               (|_Perceive10044| |primitive-actions| |_Perceive10044|)
               (|_Perceive10044| |time-during| |_Time-Interval10050|)
               (|_Perceive10044| |object| |_Entity10048|))
              T NIL)))
(|Radius-Scale| (|_Radius-Scale37542|
                 (((|_Radius-Scale37542| |instance-of| |Radius-Scale|)
                   (|_Number37544| |instance-of| |Number|)
                   (|_Radius-Scale37542| |number-of-elements|
                    |_Number37544|))
                  T NIL)))
(|Learning| (|_Learning767|
             (((|_Learning767| |instance-of| |Learning|)
               (|_Time-Interval773| |instance-of| |Time-Interval|)
               (|_Learning767| |actions| |_Learning767|)
               (|_Learning767| |primitive-actions| |_Learning767|)
               (|_Learning767| |time-during| |_Time-Interval773|))
              T NIL)))
(|Be-Activated| (|_Be-Activated403|
                 (((|_Time-Interval407| |instance-of| |Time-Interval|)
                   (|_Be-Activated403| |instance-of| |Be-Activated|)
                   (|_Entity406| |instance-of| |Entity|)
                   (|_Be-Activated403| |actions| |_Be-Activated403|)
                   (|_Be-Activated403| |primitive-actions|
                    |_Be-Activated403|)
                   (|_Be-Activated403| |time-during|
                    |_Time-Interval407|)
                   (|_Be-Activated403| |object| |_Entity406|))
                  T NIL)))
(|Recognize| (|_Recognize10067|
              (((|_Move10074| |instance-of| |Move|)
                (|_Time-Interval10073| |instance-of| |Time-Interval|)
                (|_Recognize10067| |instance-of| |Recognize|)
                (|_Entity10071| |instance-of| |Spatial-Entity|)
                (|_Recognize10067| |actions| |_Recognize10067|)
                (|_Recognize10067| |preparatory-event| |_Move10074|)
                (|_Recognize10067| |primitive-actions|
                 |_Recognize10067|)
                (|_Recognize10067| |time-during| |_Time-Interval10073|)
                (|_Recognize10067| |object| |_Entity10071|))
               T NIL)))
(|Be-Closed| (|_Be-Closed318|
              (((|_Portal-Covering324| |instance-of| |Portal-Covering|)
                (|_Time-Interval326| |instance-of| |Time-Interval|)
                (|_Spatial-Entity325| |instance-of| |Spatial-Entity|)
                (|_Be-Closed318| |instance-of| |Be-Closed|)
                (|_Tangible-Entity323| |instance-of| |Tangible-Entity|)
                (|_Tangible-Entity323| |plays| |_Portal-Covering324|)
                (|_Be-Closed318| |actions| |_Be-Closed318|)
                (|_Be-Closed318| |primitive-actions| |_Be-Closed318|)
                (|_Be-Closed318| |time-during| |_Time-Interval326|)
                (|_Be-Closed318| |object| |_Spatial-Entity325|)
                (|_Be-Closed318| |instrument| |_Tangible-Entity323|))
               T NIL)))
(|Carrier| (|_Carrier33118|
            (((|_Transmit33121| |instance-of| |Transmit|)
              (|_Carrier33118| |instance-of| |Carrier|)
              (|_Entity33120| |instance-of| |Entity|)
              (|_Carrier33118| |in-event| |_Transmit33121|)
              (|_Carrier33118| |played-by| |_Entity33120|))
             T NIL)))
(|Barrier| (|_Barrier33134|
            (((|_Block33136| |instance-of| |Block|)
              (|_Barrier33134| |instance-of| |Barrier|)
              (|_Tangible-Entity33135| |instance-of| |Tangible-Entity|)
              (|_Barrier33134| |in-event| |_Block33136|)
              (|_Barrier33134| |played-by| |_Tangible-Entity33135|))
             T NIL)))
(|Age-Scale| (|_Age-Scale37652|
              (((|_Age-Scale37652| |instance-of| |Age-Scale|)
                (|_Number37654| |instance-of| |Number|)
                (|_Age-Scale37652| |number-of-elements|
                 |_Number37654|))
               T NIL)))
(|Be-Open| (|_Be-Open393|
            (((|_Portal398| |instance-of| |Portal|)
              (|_Time-Interval399| |instance-of| |Time-Interval|)
              (|_Be-Open393| |instance-of| |Be-Open|)
              (|_Spatial-Entity397| |instance-of| |Spatial-Entity|)
              (|_Spatial-Entity397| |plays| |_Portal398|)
              (|_Be-Open393| |actions| |_Be-Open393|)
              (|_Be-Open393| |primitive-actions| |_Be-Open393|)
              (|_Be-Open393| |time-during| |_Time-Interval399|)
              (|_Be-Open393| |object| |_Spatial-Entity397|))
             T NIL)))
(|Instruction| (|_Instruction33010|
                (((|_Action33013| |instance-of| |Action|)
                  (|_Instruction33010| |instance-of| |Instruction|)
                  (|_Information33012| |instance-of| |Information|)
                  (|_Instruction33010| |in-event| |_Action33013|)
                  (|_Instruction33010| |played-by|
                   |_Information33012|))
                 T NIL)))
(|Ice| (|_Ice34619| (((|_Ice34619| |instance-of| |Ice|)) T NIL)))
(|Consistency-Constant| (|_Consistency-Constant37903|
                         (((|_Consistency-Constant37903| |instance-of|
                            |Consistency-Constant|))
                          T NIL)))
(|Mass-Constant| (|_Mass-Constant37861|
                  (((|_Mass-Constant37861| |instance-of|
                     |Mass-Constant|))
                   T NIL)))
(|Block-Object| (|_Block-Object34601|
                 (((|_Block-Object34601| |instance-of| |Block-Object|))
                  T NIL)))
(|Consistency-Value| (|_Consistency-Value37747|
                      (((|_Consistency-Value37747| |instance-of|
                         |Consistency-Value|))
                       T NIL)))
(|Let-Fall| (|_Let-Fall24293|
             (((|_Time-Interval24337| |instance-of| |Time-Interval|)
               (|_Entity24333| |instance-of| |Entity|)
               (|_Move24331| |instance-of| |Move|)
               (|_Make-Accessible24330| |instance-of|
                |Make-Accessible|)
               (|_Time-Interval24329| |instance-of| |Time-Interval|)
               (|_Be-Inaccessible24328| |instance-of|
                |Be-Inaccessible|)
               (|_Move24304| |instance-of| |Move|)
               (|_Make-Inaccessible24303| |instance-of|
                |Make-Inaccessible|)
               (|_Time-Interval24302| |instance-of| |Time-Interval|)
               (|_Tangible-Entity24300| |instance-of|
                |Tangible-Entity|)
               (|_Fall24301| |instance-of| |Fall|)
               (|_Let-Fall24293| |instance-of| |Let-Fall|)
               (|_Tangible-Entity24298| |instance-of|
                |Tangible-Entity|)
               (|_Be-Inaccessible24328| |actions|
                |_Be-Inaccessible24328|)
               (|_Be-Inaccessible24328| |primitive-actions|
                |_Be-Inaccessible24328|)
               (|_Be-Inaccessible24328| |time-during|
                |_Time-Interval24337|)
               (|_Be-Inaccessible24328| |object| |_Entity24333|)
               (|_Make-Inaccessible24303| |actions|
                |_Make-Inaccessible24303|)
               (|_Make-Inaccessible24303| |preparatory-event|
                |_Move24331|)
               (|_Make-Inaccessible24303| |preparatory-event|
                |_Make-Accessible24330|)
               (|_Make-Inaccessible24303| |primitive-actions|
                |_Make-Inaccessible24303|)
               (|_Make-Inaccessible24303| |time-during|
                |_Time-Interval24329|)
               (|_Make-Inaccessible24303| |object|
                |_Tangible-Entity24300|)
               (|_Make-Inaccessible24303| |resulting-state|
                |_Be-Inaccessible24328|)
               (|_Make-Inaccessible24303| |agent|
                |_Tangible-Entity24298|)
               (|_Let-Fall24293| |actions| |_Let-Fall24293|)
               (|_Let-Fall24293| |preparatory-event| |_Move24304|)
               (|_Let-Fall24293| |preparatory-event|
                |_Make-Inaccessible24303|)
               (|_Let-Fall24293| |primitive-actions| |_Let-Fall24293|)
               (|_Let-Fall24293| |time-during| |_Time-Interval24302|)
               (|_Let-Fall24293| |object| |_Tangible-Entity24300|)
               (|_Let-Fall24293| |causes| |_Fall24301|)
               (|_Let-Fall24293| |agent| |_Tangible-Entity24298|))
              T NIL)))
(|Create| (|_Create25447|
           (((|_Time-Interval25453| |instance-of| |Time-Interval|)
             (|_Create25447| |instance-of| |Create|)
             (|_Entity25451| |instance-of| |Entity|)
             (|_Create25447| |actions| |_Create25447|)
             (|_Create25447| |primitive-actions| |_Create25447|)
             (|_Create25447| |time-during| |_Time-Interval25453|)
             (|_Create25447| |result| |_Entity25451|))
            T NIL)))
(|Manner-Constant| (|_Manner-Constant37863|
                    (((|_Manner-Constant37863| |instance-of|
                       |Manner-Constant|))
                     T NIL)))
(|Release| (|_Release24213|
            (((|_Container24224| |instance-of| |Container|)
              (|_Move24223| |instance-of| |Move|)
              (|_Confine24221| |instance-of| |Confine|)
              (|_Time-Interval24220| |instance-of| |Time-Interval|)
              (|_Tangible-Entity24219| |instance-of| |Tangible-Entity|)
              (|_Release24213| |instance-of| |Release|)
              (|_Tangible-Entity24217| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity24219| |plays| |_Container24224|)
              (|_Tangible-Entity24217| |is-inside|
               |_Tangible-Entity24219|)
              (|_Release24213| |actions| |_Release24213|)
              (|_Release24213| |preparatory-event| |_Move24223|)
              (|_Release24213| |preparatory-event| |_Confine24221|)
              (|_Release24213| |primitive-actions| |_Release24213|)
              (|_Release24213| |time-during| |_Time-Interval24220|)
              (|_Release24213| |base| |_Tangible-Entity24219|)
              (|_Release24213| |object| |_Tangible-Entity24217|))
             T NIL)))
(|Move-Through| (|_Move-Through10856|
                 (((|_Barrier10914| |instance-of| |Barrier|)
                   (|_Move10901| |instance-of| |Move|)
                   (|_Time-Interval10900| |instance-of|
                    |Time-Interval|)
                   (|_Spatial-Entity10873| |instance-of|
                    |Spatial-Entity|)
                   (|_Tangible-Entity10872| |instance-of|
                    |Tangible-Entity|)
                   (|_Tangible-Entity10885| |instance-of|
                    |Tangible-Entity|)
                   (|_Acceleration-Magnitude-Value10876| |instance-of|
                    |Acceleration-Magnitude-Value|)
                   (|_Acceleration-Vector-Value10875| |instance-of|
                    |Acceleration-Vector-Value|)
                   (|_Length-Value10882| |instance-of| |Length-Value|)
                   (|_Duration-Value10874| |instance-of|
                    |Duration-Value|)
                   (|_Speed-Value10878| |instance-of| |Speed-Value|)
                   (|_Displacement-Vector-Value10881| |instance-of|
                    |Displacement-Vector-Value|)
                   (|_Speed-Value10899| |instance-of| |Speed-Value|)
                   (|_Velocity-Vector-Value10896| |instance-of|
                    |Velocity-Vector-Value|)
                   (|_Speed-Value10898| |instance-of| |Speed-Value|)
                   (|_Speed-Value10897| |instance-of| |Speed-Value|)
                   (|_Speed-Value10895| |instance-of| |Speed-Value|)
                   (|_Velocity-Vector-Value10892| |instance-of|
                    |Velocity-Vector-Value|)
                   (|_Speed-Value10894| |instance-of| |Speed-Value|)
                   (|_Speed-Value10893| |instance-of| |Speed-Value|)
                   (|_Velocity-Vector-Value10877| |instance-of|
                    |Velocity-Vector-Value|)
                   (|_Acceleration-Magnitude-Value10891| |instance-of|
                    |Acceleration-Magnitude-Value|)
                   (|_Length-Value10890| |instance-of| |Length-Value|)
                   (|_Speed-Value10889| |instance-of| |Speed-Value|)
                   (|_Acceleration-Magnitude-Value10888| |instance-of|
                    |Acceleration-Magnitude-Value|)
                   (|_Length-Value10887| |instance-of| |Length-Value|)
                   (|_Move-Through10856| |instance-of| |Move-Through|)
                   (|_Speed-Value10886| |instance-of| |Speed-Value|)
                   (|_Tangible-Entity10872| |plays| |_Barrier10914|)
                   (|_Tangible-Entity10872| |has-region|
                    |_Spatial-Entity10873|)
                   (|_Move-Through10856| |actions|
                    |_Move-Through10856|)
                   (|_Move-Through10856| |preparatory-event|
                    |_Move10901|)
                   (|_Move-Through10856| |primitive-actions|
                    |_Move-Through10856|)
                   (|_Move-Through10856| |time-during|
                    |_Time-Interval10900|)
                   (|_Move-Through10856| |path| |_Spatial-Entity10873|)
                   (|_Move-Through10856| |base|
                    |_Tangible-Entity10872|)
                   (|_Move-Through10856| |object|
                    |_Tangible-Entity10885|)
                   (|_Move-Through10856| |acceleration-magnitude|
                    |_Acceleration-Magnitude-Value10876|)
                   (|_Move-Through10856| |acceleration|
                    |_Acceleration-Vector-Value10875|)
                   (|_Move-Through10856| |distance|
                    |_Length-Value10882|)
                   (|_Move-Through10856| |duration|
                    |_Duration-Value10874|)
                   (|_Move-Through10856| |speed| |_Speed-Value10878|)
                   (|_Move-Through10856| |displacement|
                    |_Displacement-Vector-Value10881|)
                   (|_Move-Through10856| |final-speed|
                    |_Speed-Value10899|)
                   (|_Move-Through10856| |final-velocity|
                    |_Velocity-Vector-Value10896|)
                   (|_Move-Through10856| |final-x-speed|
                    |_Speed-Value10898|)
                   (|_Move-Through10856| |final-y-speed|
                    |_Speed-Value10897|)
                   (|_Move-Through10856| |initial-speed|
                    |_Speed-Value10895|)
                   (|_Move-Through10856| |initial-velocity|
                    |_Velocity-Vector-Value10892|)
                   (|_Move-Through10856| |initial-x-speed|
                    |_Speed-Value10894|)
                   (|_Move-Through10856| |initial-y-speed|
                    |_Speed-Value10893|)
                   (|_Move-Through10856| |velocity|
                    |_Velocity-Vector-Value10877|)
                   (|_Move-Through10856| |x-acceleration-magnitude|
                    |_Acceleration-Magnitude-Value10891|)
                   (|_Move-Through10856| |x-distance|
                    |_Length-Value10890|)
                   (|_Move-Through10856| |x-speed| |_Speed-Value10889|)
                   (|_Move-Through10856| |y-acceleration-magnitude|
                    |_Acceleration-Magnitude-Value10888|)
                   (|_Move-Through10856| |y-distance|
                    |_Length-Value10887|)
                   (|_Move-Through10856| |y-speed|
                    |_Speed-Value10886|))
                  T NIL)))
(|Water| (|_Water33890|
          (((|_Categorical33895| |instance-of| |Categorical|)
            (|_Water33890| |instance-of| |Water|)
            (|_State-Value33894| |instance-of| |State-Value|)
            (|_State-Value33894| |categorical-value|
             |_Categorical33895|)
            (|_Water33890| |physical-state| |_State-Value33894|))
           T NIL)))
(|Trespass| (|_Trespass35213|
             (((|_Entity35282| |instance-of| |Entity|)
               (|_Leave35259| |instance-of| |Leave|)
               (|_Time-Interval35258| |instance-of| |Time-Interval|)
               (|_Spatial-Entity35230| |instance-of| |Spatial-Entity|)
               (|_Event35257| |instance-of| |Event|)
               (|_Tangible-Entity35229| |instance-of|
                |Tangible-Entity|)
               (|_Acceleration-Magnitude-Value35233| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Acceleration-Vector-Value35232| |instance-of|
                |Acceleration-Vector-Value|)
               (|_Length-Value35239| |instance-of| |Length-Value|)
               (|_Duration-Value35231| |instance-of| |Duration-Value|)
               (|_Speed-Value35235| |instance-of| |Speed-Value|)
               (|_Displacement-Vector-Value35238| |instance-of|
                |Displacement-Vector-Value|)
               (|_Speed-Value35256| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value35253| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value35255| |instance-of| |Speed-Value|)
               (|_Speed-Value35254| |instance-of| |Speed-Value|)
               (|_Speed-Value35252| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value35249| |instance-of|
                |Velocity-Vector-Value|)
               (|_Speed-Value35251| |instance-of| |Speed-Value|)
               (|_Speed-Value35250| |instance-of| |Speed-Value|)
               (|_Velocity-Vector-Value35234| |instance-of|
                |Velocity-Vector-Value|)
               (|_Acceleration-Magnitude-Value35248| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value35247| |instance-of| |Length-Value|)
               (|_Speed-Value35246| |instance-of| |Speed-Value|)
               (|_Acceleration-Magnitude-Value35245| |instance-of|
                |Acceleration-Magnitude-Value|)
               (|_Length-Value35244| |instance-of| |Length-Value|)
               (|_Trespass35213| |instance-of| |Trespass|)
               (|_Speed-Value35243| |instance-of| |Speed-Value|)
               (|_Spatial-Entity35230| |is-possessed-by|
                |_Entity35282|)
               (|_Trespass35213| |actions| |_Trespass35213|)
               (|_Trespass35213| |preparatory-event| |_Leave35259|)
               (|_Trespass35213| |primitive-actions| |_Trespass35213|)
               (|_Trespass35213| |time-during| |_Time-Interval35258|)
               (|_Trespass35213| |destination| |_Spatial-Entity35230|)
               (|_Trespass35213| |object| |_Tangible-Entity35229|)
               (|_Trespass35213| |inhibited-by| |_Event35257|)
               (|_Trespass35213| |prevented-by| |_Event35257|)
               (|_Trespass35213| |agent| |_Tangible-Entity35229|)
               (|_Trespass35213| |acceleration-magnitude|
                |_Acceleration-Magnitude-Value35233|)
               (|_Trespass35213| |acceleration|
                |_Acceleration-Vector-Value35232|)
               (|_Trespass35213| |distance| |_Length-Value35239|)
               (|_Trespass35213| |duration| |_Duration-Value35231|)
               (|_Trespass35213| |speed| |_Speed-Value35235|)
               (|_Trespass35213| |displacement|
                |_Displacement-Vector-Value35238|)
               (|_Trespass35213| |final-speed| |_Speed-Value35256|)
               (|_Trespass35213| |final-velocity|
                |_Velocity-Vector-Value35253|)
               (|_Trespass35213| |final-x-speed| |_Speed-Value35255|)
               (|_Trespass35213| |final-y-speed| |_Speed-Value35254|)
               (|_Trespass35213| |initial-speed| |_Speed-Value35252|)
               (|_Trespass35213| |initial-velocity|
                |_Velocity-Vector-Value35249|)
               (|_Trespass35213| |initial-x-speed| |_Speed-Value35251|)
               (|_Trespass35213| |initial-y-speed| |_Speed-Value35250|)
               (|_Trespass35213| |velocity|
                |_Velocity-Vector-Value35234|)
               (|_Trespass35213| |x-acceleration-magnitude|
                |_Acceleration-Magnitude-Value35248|)
               (|_Trespass35213| |x-distance| |_Length-Value35247|)
               (|_Trespass35213| |x-speed| |_Speed-Value35246|)
               (|_Trespass35213| |y-acceleration-magnitude|
                |_Acceleration-Magnitude-Value35245|)
               (|_Trespass35213| |y-distance| |_Length-Value35244|)
               (|_Trespass35213| |y-speed| |_Speed-Value35243|))
              T NIL)))
(|Acceleration-Magnitude-Scale| (|_Acceleration-Magnitude-Scale37657|
                                 (((|_Acceleration-Magnitude-Scale37657|
                                    |instance-of|
                                    |Acceleration-Magnitude-Scale|)
                                   (|_Number37659|
                                    |instance-of|
                                    |Number|)
                                   (|_Acceleration-Magnitude-Scale37657|
                                    |number-of-elements|
                                    |_Number37659|))
                                  T
                                  NIL)))
(|Paper| (|_Paper33794|
          (((|_Categorical33799| |instance-of| |Categorical|)
            (|_Paper33794| |instance-of| |Paper|)
            (|_State-Value33798| |instance-of| |State-Value|)
            (|_State-Value33798| |categorical-value|
             |_Categorical33799|)
            (|_Paper33794| |physical-state| |_State-Value33798|))
           T NIL)))
(|Version| (|_Version34940|
            (((|_Version34940| |instance-of| |Version|)) T NIL)))
(|Inorganic-Molecule| (|_Inorganic-Molecule33993|
                       (((|_Inorganic-Molecule33993| |instance-of|
                          |Inorganic-Molecule|)
                         (|_Atom33995| |instance-of| |Atom|)
                         (|_Inorganic-Molecule33993|
                          |has-basic-structural-unit| |_Atom33995|))
                        T NIL)))
(|UoM-Coordinate| (|_UoM-Coordinate37470|
                   (((|_UoM-Coordinate37470| |instance-of|
                      |UoM-Coordinate|))
                    T NIL)))
(|KM-Class| (|_KM-Class34918|
             (((|_KM-Class34918| |instance-of| |KM-Class|)) T NIL)))
(|Slope-Scale| (|_Slope-Scale37527|
                (((|_Slope-Scale37527| |instance-of| |Slope-Scale|)
                  (|_Number37529| |instance-of| |Number|)
                  (|_Slope-Scale37527| |number-of-elements|
                   |_Number37529|))
                 T NIL)))
(|Team| (|_Team34857|
         (((|_Team34857| |instance-of| |Team|)
           (|_Number34859| |instance-of| |Number|)
           (|_Team34857| |number-of-elements| |_Number34859|))
          T NIL)))
(|Money| (|_Money34730|
          (((|_Resource34733| |instance-of| |Resource|)
            (|_Resource34732| |instance-of| |Resource|)
            (|_Money34730| |instance-of| |Money|)
            (|_Worth-Value34731| |instance-of| |Worth-Value|)
            (|_Money34730| |purpose| |_Resource34733|)
            (|_Money34730| |purpose| |_Resource34732|)
            (|_Money34730| |worth| |_Worth-Value34731|))
           T NIL)))
(|Exit| (|_Exit37122|
         (((|_Move37187| |instance-of| |Move|)
           (|_Time-Interval37186| |instance-of| |Time-Interval|)
           (|_Acceleration-Magnitude-Value37185| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value37169| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value37184| |instance-of| |Length-Value|)
           (|_Duration-Value37183| |instance-of| |Duration-Value|)
           (|_Speed-Value37182| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value37167| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value37181| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value37178| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value37180| |instance-of| |Speed-Value|)
           (|_Speed-Value37179| |instance-of| |Speed-Value|)
           (|_Speed-Value37177| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value37174| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value37176| |instance-of| |Speed-Value|)
           (|_Speed-Value37175| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value37165| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value37173| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value37172| |instance-of| |Length-Value|)
           (|_Speed-Value37171| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value37170| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value37168| |instance-of| |Length-Value|)
           (|_Speed-Value37166| |instance-of| |Speed-Value|)
           (|_Move37164| |instance-of| |Move|)
           (|_Admit37162| |instance-of| |Admit|)
           (|_Time-Interval37161| |instance-of| |Time-Interval|)
           (|_Be-Shut-Out37160| |instance-of| |Be-Shut-Out|)
           (|_Portal37159| |instance-of| |Portal|)
           (|_Barrier37158| |instance-of| |Container|)
           (|_Angle-Value37157| |instance-of| |Angle-Value|)
           (|_Angle-Value37156| |instance-of| |Angle-Value|)
           (|_Angle-Value37155| |instance-of| |Angle-Value|)
           (|_Angle-Value37154| |instance-of| |Angle-Value|)
           (|_Angle-Value37153| |instance-of| |Angle-Value|)
           (|_Move37152| |instance-of| |Move|)
           (|_Shut-Out37151| |instance-of| |Shut-Out|)
           (|_Time-Interval37150| |instance-of| |Time-Interval|)
           (|_Spatial-Entity37146| |instance-of| |Spatial-Entity|)
           (|_Spatial-Entity37145| |instance-of| |Spatial-Entity|)
           (|_Spatial-Entity37147| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity37148| |instance-of| |Tangible-Entity|)
           (|_Tangible-Entity37144| |instance-of| |Tangible-Entity|)
           (|_Acceleration-Magnitude-Value37143| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value37127| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value37142| |instance-of| |Length-Value|)
           (|_Duration-Value37141| |instance-of| |Duration-Value|)
           (|_Speed-Value37140| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value37125| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value37139| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value37136| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value37138| |instance-of| |Speed-Value|)
           (|_Speed-Value37137| |instance-of| |Speed-Value|)
           (|_Speed-Value37135| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value37132| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value37134| |instance-of| |Speed-Value|)
           (|_Speed-Value37133| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value37123| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value37131| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value37130| |instance-of| |Length-Value|)
           (|_Speed-Value37129| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value37128| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value37126| |instance-of| |Length-Value|)
           (|_Exit37122| |instance-of| |Exit|)
           (|_Speed-Value37124| |instance-of| |Speed-Value|)
           (|_Move37152| |actions| |_Move37152|)
           (|_Move37152| |preparatory-event| |_Move37187|)
           (|_Move37152| |primitive-actions| |_Move37152|)
           (|_Move37152| |time-during| |_Time-Interval37186|)
           (|_Move37152| |destination| |_Spatial-Entity37145|)
           (|_Move37152| |object| |_Tangible-Entity37144|)
           (|_Move37152| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value37185|)
           (|_Move37152| |acceleration|
            |_Acceleration-Vector-Value37169|)
           (|_Move37152| |distance| |_Length-Value37184|)
           (|_Move37152| |duration| |_Duration-Value37183|)
           (|_Move37152| |speed| |_Speed-Value37182|)
           (|_Move37152| |displacement|
            |_Displacement-Vector-Value37167|)
           (|_Move37152| |final-speed| |_Speed-Value37181|)
           (|_Move37152| |final-velocity|
            |_Velocity-Vector-Value37178|)
           (|_Move37152| |final-x-speed| |_Speed-Value37180|)
           (|_Move37152| |final-y-speed| |_Speed-Value37179|)
           (|_Move37152| |initial-speed| |_Speed-Value37177|)
           (|_Move37152| |initial-velocity|
            |_Velocity-Vector-Value37174|)
           (|_Move37152| |initial-x-speed| |_Speed-Value37176|)
           (|_Move37152| |initial-y-speed| |_Speed-Value37175|)
           (|_Move37152| |velocity| |_Velocity-Vector-Value37165|)
           (|_Move37152| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value37173|)
           (|_Move37152| |x-distance| |_Length-Value37172|)
           (|_Move37152| |x-speed| |_Speed-Value37171|)
           (|_Move37152| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value37170|)
           (|_Move37152| |y-distance| |_Length-Value37168|)
           (|_Move37152| |y-speed| |_Speed-Value37166|)
           (|_Shut-Out37151| |actions| |_Shut-Out37151|)
           (|_Shut-Out37151| |preparatory-event| |_Move37164|)
           (|_Shut-Out37151| |preparatory-event| |_Admit37162|)
           (|_Shut-Out37151| |primitive-actions| |_Shut-Out37151|)
           (|_Shut-Out37151| |time-during| |_Time-Interval37161|)
           (|_Shut-Out37151| |base| |_Tangible-Entity37148|)
           (|_Shut-Out37151| |object| |_Tangible-Entity37144|)
           (|_Shut-Out37151| |resulting-state| |_Be-Shut-Out37160|)
           (|_Spatial-Entity37146| |is-outside|
            |_Tangible-Entity37148|)
           (|_Spatial-Entity37147| |plays| |_Portal37159|)
           (|_Tangible-Entity37148| |encloses| |_Spatial-Entity37145|)
           (|_Tangible-Entity37148| |plays| |_Barrier37158|)
           (|_Tangible-Entity37148| |has-region|
            |_Spatial-Entity37147|)
           (|_Acceleration-Vector-Value37127| |x-component-slot|
            |x-acceleration-magnitude|)
           (|_Acceleration-Vector-Value37127| |y-component-slot|
            |y-acceleration-magnitude|)
           (|_Acceleration-Vector-Value37127| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value37143|)
           (|_Acceleration-Vector-Value37127| |direction|
            |_Angle-Value37157|)
           (|_Displacement-Vector-Value37125| |x-component-slot|
            |x-distance|)
           (|_Displacement-Vector-Value37125| |y-component-slot|
            |y-distance|)
           (|_Displacement-Vector-Value37125| |direction|
            |_Angle-Value37156|)
           (|_Displacement-Vector-Value37125| |distance|
            |_Length-Value37142|)
           (|_Velocity-Vector-Value37136| |x-component-slot| |x-speed|)
           (|_Velocity-Vector-Value37136| |y-component-slot| |y-speed|)
           (|_Velocity-Vector-Value37136| |direction|
            |_Angle-Value37155|)
           (|_Velocity-Vector-Value37136| |speed| |_Speed-Value37139|)
           (|_Speed-Value37138| |x-speed-of|
            |_Velocity-Vector-Value37136|)
           (|_Speed-Value37137| |y-speed-of|
            |_Velocity-Vector-Value37136|)
           (|_Velocity-Vector-Value37132| |x-component-slot| |x-speed|)
           (|_Velocity-Vector-Value37132| |y-component-slot| |y-speed|)
           (|_Velocity-Vector-Value37132| |direction|
            |_Angle-Value37154|)
           (|_Velocity-Vector-Value37132| |speed| |_Speed-Value37135|)
           (|_Speed-Value37134| |x-speed-of|
            |_Velocity-Vector-Value37132|)
           (|_Speed-Value37133| |y-speed-of|
            |_Velocity-Vector-Value37132|)
           (|_Velocity-Vector-Value37123| |x-component-slot| |x-speed|)
           (|_Velocity-Vector-Value37123| |y-component-slot| |y-speed|)
           (|_Velocity-Vector-Value37123| |direction|
            |_Angle-Value37153|)
           (|_Velocity-Vector-Value37123| |speed| |_Speed-Value37140|)
           (|_Acceleration-Magnitude-Value37131|
            |x-acceleration-magnitude-of|
            |_Acceleration-Vector-Value37127|)
           (|_Length-Value37130| |x-distance-of|
            |_Displacement-Vector-Value37125|)
           (|_Speed-Value37129| |x-speed-of|
            |_Velocity-Vector-Value37123|)
           (|_Acceleration-Magnitude-Value37128|
            |y-acceleration-magnitude-of|
            |_Acceleration-Vector-Value37127|)
           (|_Length-Value37126| |y-distance-of|
            |_Displacement-Vector-Value37125|)
           (|_Speed-Value37124| |y-speed-of|
            |_Velocity-Vector-Value37123|)
           (|_Exit37122| |actions| |_Exit37122|)
           (|_Exit37122| |preparatory-event| |_Move37152|)
           (|_Exit37122| |preparatory-event| |_Shut-Out37151|)
           (|_Exit37122| |primitive-actions| |_Exit37122|)
           (|_Exit37122| |time-during| |_Time-Interval37150|)
           (|_Exit37122| |destination| |_Spatial-Entity37146|)
           (|_Exit37122| |origin| |_Spatial-Entity37145|)
           (|_Exit37122| |path| |_Spatial-Entity37147|)
           (|_Exit37122| |base| |_Tangible-Entity37148|)
           (|_Exit37122| |object| |_Tangible-Entity37144|)
           (|_Exit37122| |agent| |_Tangible-Entity37144|)
           (|_Exit37122| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value37143|)
           (|_Exit37122| |acceleration|
            |_Acceleration-Vector-Value37127|)
           (|_Exit37122| |distance| |_Length-Value37142|)
           (|_Exit37122| |duration| |_Duration-Value37141|)
           (|_Exit37122| |speed| |_Speed-Value37140|)
           (|_Exit37122| |displacement|
            |_Displacement-Vector-Value37125|)
           (|_Exit37122| |final-speed| |_Speed-Value37139|)
           (|_Exit37122| |final-velocity|
            |_Velocity-Vector-Value37136|)
           (|_Exit37122| |final-x-speed| |_Speed-Value37138|)
           (|_Exit37122| |final-y-speed| |_Speed-Value37137|)
           (|_Exit37122| |initial-speed| |_Speed-Value37135|)
           (|_Exit37122| |initial-velocity|
            |_Velocity-Vector-Value37132|)
           (|_Exit37122| |initial-x-speed| |_Speed-Value37134|)
           (|_Exit37122| |initial-y-speed| |_Speed-Value37133|)
           (|_Exit37122| |velocity| |_Velocity-Vector-Value37123|)
           (|_Exit37122| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value37131|)
           (|_Exit37122| |x-distance| |_Length-Value37130|)
           (|_Exit37122| |x-speed| |_Speed-Value37129|)
           (|_Exit37122| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value37128|)
           (|_Exit37122| |y-distance| |_Length-Value37126|)
           (|_Exit37122| |y-speed| |_Speed-Value37124|))
          NIL NIL)))
(|Length-Scale| (|_Length-Scale37567|
                 (((|_Length-Scale37567| |instance-of| |Length-Scale|)
                   (|_Number37569| |instance-of| |Number|)
                   (|_Length-Scale37567| |number-of-elements|
                    |_Number37569|))
                  T NIL)))
(|Voltage-Constant| (|_Voltage-Constant37807|
                     (((|_Voltage-Constant37807| |instance-of|
                        |Voltage-Constant|))
                      T NIL)))
(|Size-Value| (|_Size-Value37698|
               (((|_Size-Value37698| |instance-of| |Size-Value|)) T
                NIL)))
(|Breakability-Constant| (|_Breakability-Constant37913|
                          (((|_Breakability-Constant37913|
                             |instance-of| |Breakability-Constant|))
                           T NIL)))
(|Destroy| (|_Destroy24952|
            (((|_Time-Interval24966| |instance-of| |Time-Interval|)
              (|_Physical-Object24962| |instance-of| |Physical-Object|)
              (|_Move24960| |instance-of| |Move|)
              (|_Time-Interval24959| |instance-of| |Time-Interval|)
              (|_Tangible-Entity24956| |instance-of| |Physical-Object|)
              (|_Destroy24952| |instance-of| |Destroy|)
              (|_Be-Broken24958| |instance-of| |Be-Broken|)
              (|_Be-Broken24958| |actions| |_Be-Broken24958|)
              (|_Be-Broken24958| |primitive-actions| |_Be-Broken24958|)
              (|_Be-Broken24958| |time-during| |_Time-Interval24966|)
              (|_Be-Broken24958| |object| |_Physical-Object24962|)
              (|_Destroy24952| |actions| |_Destroy24952|)
              (|_Destroy24952| |preparatory-event| |_Move24960|)
              (|_Destroy24952| |primitive-actions| |_Destroy24952|)
              (|_Destroy24952| |time-during| |_Time-Interval24959|)
              (|_Destroy24952| |object| |_Tangible-Entity24956|)
              (|_Destroy24952| |resulting-state| |_Be-Broken24958|))
             T NIL)))
(|Obstruct| (|_Obstruct23603|
             (((|_Move23627| |instance-of| |Move|)
               (|_Make-Inaccessible23626| |instance-of|
                |Make-Inaccessible|)
               (|_Time-Interval23625| |instance-of| |Time-Interval|)
               (|_Time-Interval23618| |instance-of| |Time-Interval|)
               (|_Entity23614| |instance-of| |Tangible-Entity|)
               (|_Move23612| |instance-of| |Move|)
               (|_Make-Accessible23611| |instance-of|
                |Make-Accessible|)
               (|_Time-Interval23610| |instance-of| |Time-Interval|)
               (|_Tangible-Entity23607| |instance-of|
                |Tangible-Entity|)
               (|_Obstruct23603| |instance-of| |Obstruct|)
               (|_Be-Obstructed23609| |instance-of| |Be-Obstructed|)
               (|_Make-Accessible23611| |actions|
                |_Make-Accessible23611|)
               (|_Make-Accessible23611| |preparatory-event|
                |_Move23627|)
               (|_Make-Accessible23611| |preparatory-event|
                |_Make-Inaccessible23626|)
               (|_Make-Accessible23611| |primitive-actions|
                |_Make-Accessible23611|)
               (|_Make-Accessible23611| |time-during|
                |_Time-Interval23625|)
               (|_Make-Accessible23611| |object|
                |_Tangible-Entity23607|)
               (|_Be-Obstructed23609| |actions| |_Be-Obstructed23609|)
               (|_Be-Obstructed23609| |primitive-actions|
                |_Be-Obstructed23609|)
               (|_Be-Obstructed23609| |time-during|
                |_Time-Interval23618|)
               (|_Be-Obstructed23609| |object| |_Entity23614|)
               (|_Obstruct23603| |actions| |_Obstruct23603|)
               (|_Obstruct23603| |preparatory-event| |_Move23612|)
               (|_Obstruct23603| |preparatory-event|
                |_Make-Accessible23611|)
               (|_Obstruct23603| |primitive-actions| |_Obstruct23603|)
               (|_Obstruct23603| |time-during| |_Time-Interval23610|)
               (|_Obstruct23603| |object| |_Tangible-Entity23607|)
               (|_Obstruct23603| |resulting-state|
                |_Be-Obstructed23609|))
              T NIL)))
(|Breach| (|_Breach24984|
           (((|_Portal25000| |instance-of| |Portal|)
             (|_Time-Interval24997| |instance-of| |Time-Interval|)
             (|_Physical-Object24992| |instance-of| |Physical-Object|)
             (|_Move24991| |instance-of| |Move|)
             (|_Time-Interval24990| |instance-of| |Time-Interval|)
             (|_Tangible-Entity24985| |instance-of| |Physical-Object|)
             (|_Spatial-Entity24987| |instance-of| |Spatial-Entity|)
             (|_Breach24984| |instance-of| |Breach|)
             (|_Be-Broken24989| |instance-of| |Be-Broken|)
             (|_Spatial-Entity24987| |plays| |_Portal25000|)
             (|_Be-Broken24989| |actions| |_Be-Broken24989|)
             (|_Be-Broken24989| |primitive-actions| |_Be-Broken24989|)
             (|_Be-Broken24989| |time-during| |_Time-Interval24997|)
             (|_Be-Broken24989| |object| |_Physical-Object24992|)
             (|_Breach24984| |actions| |_Breach24984|)
             (|_Breach24984| |preparatory-event| |_Move24991|)
             (|_Breach24984| |primitive-actions| |_Breach24984|)
             (|_Breach24984| |time-during| |_Time-Interval24990|)
             (|_Breach24984| |object| |_Tangible-Entity24985|)
             (|_Breach24984| |result| |_Spatial-Entity24987|)
             (|_Breach24984| |resulting-state| |_Be-Broken24989|))
            T NIL)))
(|Property-Node| (|_Property-Node34904|
                  (((|_Property-Node34904| |instance-of|
                     |Property-Node|))
                   T NIL)))
(|Volume-Value| (|_Volume-Value37727|
                 (((|_Volume-Value37727| |instance-of| |Volume-Value|))
                  T NIL)))
(|Agent-Role| (|_Agent-Role33540|
               (((|_Agent-Role33540| |instance-of| |Agent-Role|)
                 (|_Entity33541| |instance-of| |Entity|)
                 (|_Agent-Role33540| |played-by| |_Entity33541|))
                T NIL)))
(|Be-Contained| (|_Be-Contained364|
                 (((|_Time-Interval369| |instance-of| |Time-Interval|)
                   (|_Spatial-Entity366| |instance-of|
                    |Spatial-Entity|)
                   (|_Spatial-Entity365| |instance-of|
                    |Spatial-Entity|)
                   (|_Tangible-Entity368| |instance-of|
                    |Tangible-Entity|)
                   (|_Be-Contained364| |instance-of| |Be-Contained|)
                   (|_Tangible-Entity367| |instance-of|
                    |Tangible-Entity|)
                   (|_Be-Contained364| |actions| |_Be-Contained364|)
                   (|_Be-Contained364| |primitive-actions|
                    |_Be-Contained364|)
                   (|_Be-Contained364| |time-during|
                    |_Time-Interval369|)
                   (|_Be-Contained364| |destination|
                    |_Spatial-Entity366|)
                   (|_Be-Contained364| |origin| |_Spatial-Entity365|)
                   (|_Be-Contained364| |base| |_Tangible-Entity368|)
                   (|_Be-Contained364| |object| |_Tangible-Entity367|))
                  T NIL)))
(|Medium| (|_Medium32999|
           (((|_Action33002| |instance-of| |Action|)
             (|_Medium32999| |instance-of| |Medium|)
             (|_Tangible-Entity33001| |instance-of| |Tangible-Entity|)
             (|_Medium32999| |in-event| |_Action33002|)
             (|_Medium32999| |played-by| |_Tangible-Entity33001|))
            T NIL)))
(|Atom| (|_Atom33996|
         (((|_Cardinal33998| |instance-of| |Cardinal|)
           (|_Number33999| |instance-of| |Number|)
           (|_Atom33996| |instance-of| |Atom|)
           (|_Charge-Value33997| |instance-of| |Charge-Value|)
           (|_Charge-Value33997| |cardinal-value| |_Cardinal33998|)
           (|_Atom33996| |chemical-period| |_Number33999|)
           (|_Atom33996| |charge| |_Charge-Value33997|))
          T NIL)))
(|UoM-Density| (|_UoM-Density37468|
                (((|_UoM-Density37468| |instance-of| |UoM-Density|)) T
                 NIL)))
(|Be-Touching| (|_Be-Touching40|
                (((|_Be-Touching40| |instance-of| |Be-Touching|)
                  (|_Time-Interval42| |instance-of| |Time-Interval|)
                  (|_Be-Touching40| |actions| |_Be-Touching40|)
                  (|_Be-Touching40| |primitive-actions|
                   |_Be-Touching40|)
                  (|_Be-Touching40| |time-during| |_Time-Interval42|))
                 T NIL)))
(|Entrance| (|_Entrance33095|
             (((|_Move-Through33098| |instance-of| |Move-Into|)
               (|_Entrance33095| |instance-of| |Entrance|)
               (|_Spatial-Entity33097| |instance-of| |Spatial-Entity|)
               (|_Entrance33095| |in-event| |_Move-Through33098|)
               (|_Entrance33095| |played-by| |_Spatial-Entity33097|))
              T NIL)))
(|Set-Aggregation-Slot| (|_Set-Aggregation-Slot16|
                         (((|_Set-Aggregation-Slot16| |instance-of|
                            |Set-Aggregation-Slot|))
                          T NIL)))
(|Impair| (|_Impair24749|
           (((|_Move24756| |instance-of| |Move|)
             (|_Time-Interval24755| |instance-of| |Time-Interval|)
             (|_Impair24749| |instance-of| |Impair|)
             (|_Tangible-Entity24753| |instance-of| |Tangible-Entity|)
             (|_Impair24749| |actions| |_Impair24749|)
             (|_Impair24749| |preparatory-event| |_Move24756|)
             (|_Impair24749| |primitive-actions| |_Impair24749|)
             (|_Impair24749| |time-during| |_Time-Interval24755|)
             (|_Impair24749| |object| |_Tangible-Entity24753|))
            T NIL)))
(|Arithmetic-Quotient| (|_Arithmetic-Quotient34968|
                        (((|_Arithmetic-Quotient34968| |instance-of|
                           |Arithmetic-Quotient|))
                         T NIL)))
(|Go-Through| (|_Go-Through35361|
               (((|_Barrier35414| |instance-of| |Barrier|)
                 (|_Time-Interval35391| |instance-of| |Time-Interval|)
                 (|_Spatial-Entity35364| |instance-of|
                  |Spatial-Entity|)
                 (|_Tangible-Entity35363| |instance-of|
                  |Tangible-Entity|)
                 (|_Tangible-Entity35362| |instance-of|
                  |Tangible-Entity|)
                 (|_Acceleration-Magnitude-Value35367| |instance-of|
                  |Acceleration-Magnitude-Value|)
                 (|_Acceleration-Vector-Value35366| |instance-of|
                  |Acceleration-Vector-Value|)
                 (|_Length-Value35373| |instance-of| |Length-Value|)
                 (|_Duration-Value35365| |instance-of|
                  |Duration-Value|)
                 (|_Speed-Value35369| |instance-of| |Speed-Value|)
                 (|_Displacement-Vector-Value35372| |instance-of|
                  |Displacement-Vector-Value|)
                 (|_Speed-Value35390| |instance-of| |Speed-Value|)
                 (|_Velocity-Vector-Value35387| |instance-of|
                  |Velocity-Vector-Value|)
                 (|_Speed-Value35389| |instance-of| |Speed-Value|)
                 (|_Speed-Value35388| |instance-of| |Speed-Value|)
                 (|_Speed-Value35386| |instance-of| |Speed-Value|)
                 (|_Velocity-Vector-Value35383| |instance-of|
                  |Velocity-Vector-Value|)
                 (|_Speed-Value35385| |instance-of| |Speed-Value|)
                 (|_Speed-Value35384| |instance-of| |Speed-Value|)
                 (|_Velocity-Vector-Value35368| |instance-of|
                  |Velocity-Vector-Value|)
                 (|_Acceleration-Magnitude-Value35382| |instance-of|
                  |Acceleration-Magnitude-Value|)
                 (|_Length-Value35381| |instance-of| |Length-Value|)
                 (|_Speed-Value35380| |instance-of| |Speed-Value|)
                 (|_Acceleration-Magnitude-Value35379| |instance-of|
                  |Acceleration-Magnitude-Value|)
                 (|_Length-Value35378| |instance-of| |Length-Value|)
                 (|_Go-Through35361| |instance-of| |Go-Through|)
                 (|_Speed-Value35377| |instance-of| |Speed-Value|)
                 (|_Tangible-Entity35363| |plays| |_Barrier35414|)
                 (|_Tangible-Entity35363| |has-region|
                  |_Spatial-Entity35364|)
                 (|_Go-Through35361| |actions| |_Go-Through35361|)
                 (|_Go-Through35361| |primitive-actions|
                  |_Go-Through35361|)
                 (|_Go-Through35361| |time-during|
                  |_Time-Interval35391|)
                 (|_Go-Through35361| |path| |_Spatial-Entity35364|)
                 (|_Go-Through35361| |base| |_Tangible-Entity35363|)
                 (|_Go-Through35361| |object| |_Tangible-Entity35362|)
                 (|_Go-Through35361| |agent| |_Tangible-Entity35362|)
                 (|_Go-Through35361| |acceleration-magnitude|
                  |_Acceleration-Magnitude-Value35367|)
                 (|_Go-Through35361| |acceleration|
                  |_Acceleration-Vector-Value35366|)
                 (|_Go-Through35361| |distance| |_Length-Value35373|)
                 (|_Go-Through35361| |duration| |_Duration-Value35365|)
                 (|_Go-Through35361| |speed| |_Speed-Value35369|)
                 (|_Go-Through35361| |displacement|
                  |_Displacement-Vector-Value35372|)
                 (|_Go-Through35361| |final-speed| |_Speed-Value35390|)
                 (|_Go-Through35361| |final-velocity|
                  |_Velocity-Vector-Value35387|)
                 (|_Go-Through35361| |final-x-speed|
                  |_Speed-Value35389|)
                 (|_Go-Through35361| |final-y-speed|
                  |_Speed-Value35388|)
                 (|_Go-Through35361| |initial-speed|
                  |_Speed-Value35386|)
                 (|_Go-Through35361| |initial-velocity|
                  |_Velocity-Vector-Value35383|)
                 (|_Go-Through35361| |initial-x-speed|
                  |_Speed-Value35385|)
                 (|_Go-Through35361| |initial-y-speed|
                  |_Speed-Value35384|)
                 (|_Go-Through35361| |velocity|
                  |_Velocity-Vector-Value35368|)
                 (|_Go-Through35361| |x-acceleration-magnitude|
                  |_Acceleration-Magnitude-Value35382|)
                 (|_Go-Through35361| |x-distance| |_Length-Value35381|)
                 (|_Go-Through35361| |x-speed| |_Speed-Value35380|)
                 (|_Go-Through35361| |y-acceleration-magnitude|
                  |_Acceleration-Magnitude-Value35379|)
                 (|_Go-Through35361| |y-distance| |_Length-Value35378|)
                 (|_Go-Through35361| |y-speed| |_Speed-Value35377|))
                T NIL)))
(|Method| (|_Method34944|
           (((|_Method34944| |instance-of| |Method|)) T NIL)))
(|UoM-Force| (|_UoM-Force37480|
              (((|_UoM-Force37480| |instance-of| |UoM-Force|)) T NIL)))
(|Dinner| (|_Dinner647|
           (((|_Time-Interval654| |instance-of| |Time-Interval|)
             (|_Dinner647| |instance-of| |Dinner|)
             (|_Eat653| |instance-of| |Eat|)
             (|_Dinner647| |actions| |_Dinner647|)
             (|_Dinner647| |actions| |_Eat653|)
             (|_Dinner647| |all-subevents| |_Eat653|)
             (|_Dinner647| |primitive-actions| |_Eat653|)
             (|_Dinner647| |time-during| |_Time-Interval654|)
             (|_Dinner647| |subevent| |_Eat653|))
            T NIL)))
(|Magnitude-Value| (|_Magnitude-Value37725|
                    (((|_Magnitude-Value37725| |instance-of|
                       |Magnitude-Value|))
                     T NIL)))
(|Seq-Aggregation-Slot| (|_Seq-Aggregation-Slot18|
                         (((|_Seq-Aggregation-Slot18| |instance-of|
                            |Seq-Aggregation-Slot|))
                          T NIL)))
(|Wetness-Constant| (|_Wetness-Constant37803|
                     (((|_Wetness-Constant37803| |instance-of|
                        |Wetness-Constant|))
                      T NIL)))
(|Shaken-Slot-Group| (|_Shaken-Slot-Group37963|
                      (((|_Shaken-Slot-Group37963| |instance-of|
                         |Shaken-Slot-Group|))
                       T NIL)))
(|SHAKEN-Slot-Group| (|_SHAKEN-Slot-Group37971|
                      (((|_SHAKEN-Slot-Group37971| |instance-of|
                         |SHAKEN-Slot-Group|))
                       T NIL)))
(|Primary-Constant| (|_Primary-Constant37849|
                     (((|_Primary-Constant37849| |instance-of|
                        |Primary-Constant|))
                      T NIL)))
(|Color-Constant| (|_Color-Constant37905|
                   (((|_Color-Constant37905| |instance-of|
                      |Color-Constant|))
                    T NIL)))
(|Suggest| (|_Suggest29450|
            (((|_Move29619| |instance-of| |Move|)
              (|_Time-Interval29618| |instance-of| |Time-Interval|)
              (|_Signal29581| |instance-of| |Signal|)
              (|_Move29568| |instance-of| |Move|)
              (|_Time-Interval29567| |instance-of| |Time-Interval|)
              (|_Time-Interval29529| |instance-of| |Time-Interval|)
              (|_Tangible-Entity29528| |instance-of| |Living-Entity|)
              (|_Entity29524| |instance-of| |Entity|)
              (|_Move29522| |instance-of| |Move|)
              (|_Time-Interval29521| |instance-of| |Time-Interval|)
              (|_Be-Known29520| |instance-of| |Be-Known|)
              (|_Move29513| |instance-of| |Move|)
              (|_Time-Interval29512| |instance-of| |Time-Interval|)
              (|_Tangible-Entity29511| |instance-of| |Tangible-Entity|)
              (|_Tangible-Entity29504| |instance-of| |Tangible-Entity|)
              (|_Move29502| |instance-of| |Move|)
              (|_Time-Interval29501| |instance-of| |Time-Interval|)
              (|_Tangible-Entity29494| |instance-of| |Tangible-Entity|)
              (|_Time-Interval29474| |instance-of| |Time-Interval|)
              (|_Tangible-Entity29473| |instance-of| |Living-Entity|)
              (|_Entity29469| |instance-of| |Entity|)
              (|_Move29467| |instance-of| |Move|)
              (|_Sense29466| |instance-of| |Sense|)
              (|_Transmit29465| |instance-of| |Transmit|)
              (|_Embody29464| |instance-of| |Embody|)
              (|_Time-Interval29463| |instance-of| |Time-Interval|)
              (|_Message29462| |instance-of| |Message|)
              (|_Message29462| |instance-of| |Spatial-Entity|)
              (|_Suggestion29457| |instance-of| |Suggestion|)
              (|_Suggestion29457| |instance-of| |Spatial-Entity|)
              (|_Tangible-Entity29455| |instance-of| |Tangible-Entity|)
              (|_Interpret29461| |instance-of| |Interpret|)
              (|_Convey29460| |instance-of| |Convey|)
              (|_Express29459| |instance-of| |Express|)
              (|_Suggest29450| |instance-of| |Suggest|)
              (|_Be-Known29458| |instance-of| |Be-Known|)
              (|_Tangible-Entity29511| |destination-of| |_Move29619|)
              (|_Sense29466| |actions-of| |_Sense29466|)
              (|_Sense29466| |preparatory-event| |_Move29619|)
              (|_Sense29466| |primitive-actions-of| |_Sense29466|)
              (|_Sense29466| |time-during| |_Time-Interval29618|)
              (|_Sense29466| |experiencer| |_Tangible-Entity29455|)
              (|_Sense29466| |object| |_Tangible-Entity29511|)
              (|_Sense29466| |result| |_Message29462|)
              (|_Sense29466| |agent| |_Tangible-Entity29455|)
              (|_Tangible-Entity29511| |object-of| |_Transmit29465|)
              (|_Tangible-Entity29511| |plays| |_Signal29581|)
              (|_Embody29464| |actions-of| |_Embody29464|)
              (|_Embody29464| |preparatory-event| |_Move29568|)
              (|_Embody29464| |primitive-actions-of| |_Embody29464|)
              (|_Embody29464| |time-during| |_Time-Interval29567|)
              (|_Embody29464| |object| |_Message29462|)
              (|_Embody29464| |result| |_Tangible-Entity29511|)
              (|_Embody29464| |next-event| |_Transmit29465|)
              (|_Embody29464| |agent| |_Tangible-Entity29504|)
              (|_Be-Known29520| |actions| |_Be-Known29520|)
              (|_Be-Known29520| |primitive-actions| |_Be-Known29520|)
              (|_Be-Known29520| |time-during| |_Time-Interval29529|)
              (|_Be-Known29520| |experiencer| |_Tangible-Entity29528|)
              (|_Be-Known29520| |object| |_Entity29524|)
              (|_Interpret29461| |actions-of| |_Interpret29461|)
              (|_Interpret29461| |preparatory-event| |_Move29522|)
              (|_Interpret29461| |primitive-actions-of|
               |_Interpret29461|)
              (|_Interpret29461| |time-during| |_Time-Interval29521|)
              (|_Interpret29461| |object| |_Message29462|)
              (|_Interpret29461| |result| |_Suggestion29457|)
              (|_Interpret29461| |resulting-state| |_Be-Known29520|)
              (|_Interpret29461| |agent| |_Tangible-Entity29455|)
              (|_Convey29460| |actions| |_Sense29466|)
              (|_Convey29460| |actions| |_Transmit29465|)
              (|_Convey29460| |actions| |_Embody29464|)
              (|_Convey29460| |actions-of| |_Convey29460|)
              (|_Convey29460| |all-subevents| |_Sense29466|)
              (|_Convey29460| |all-subevents| |_Transmit29465|)
              (|_Convey29460| |all-subevents| |_Embody29464|)
              (|_Convey29460| |preparatory-event| |_Move29513|)
              (|_Convey29460| |primitive-actions| |_Sense29466|)
              (|_Convey29460| |primitive-actions| |_Transmit29465|)
              (|_Convey29460| |primitive-actions| |_Embody29464|)
              (|_Convey29460| |time-during| |_Time-Interval29512|)
              (|_Convey29460| |base| |_Tangible-Entity29511|)
              (|_Convey29460| |object| |_Message29462|)
              (|_Convey29460| |recipient| |_Tangible-Entity29455|)
              (|_Convey29460| |first-subevent| |_Embody29464|)
              (|_Convey29460| |next-event| |_Interpret29461|)
              (|_Convey29460| |subevent| |_Sense29466|)
              (|_Convey29460| |subevent| |_Transmit29465|)
              (|_Convey29460| |subevent| |_Embody29464|)
              (|_Convey29460| |agent| |_Tangible-Entity29504|)
              (|_Express29459| |actions-of| |_Express29459|)
              (|_Express29459| |preparatory-event| |_Move29502|)
              (|_Express29459| |primitive-actions-of| |_Express29459|)
              (|_Express29459| |time-during| |_Time-Interval29501|)
              (|_Express29459| |object| |_Suggestion29457|)
              (|_Express29459| |result| |_Message29462|)
              (|_Express29459| |next-event| |_Convey29460|)
              (|_Express29459| |agent| |_Tangible-Entity29494|)
              (|_Be-Known29458| |actions| |_Be-Known29458|)
              (|_Be-Known29458| |primitive-actions| |_Be-Known29458|)
              (|_Be-Known29458| |time-during| |_Time-Interval29474|)
              (|_Be-Known29458| |experiencer| |_Tangible-Entity29473|)
              (|_Be-Known29458| |object| |_Entity29469|)
              (|_Suggest29450| |actions| |_Suggest29450|)
              (|_Suggest29450| |actions| |_Interpret29461|)
              (|_Suggest29450| |actions| |_Convey29460|)
              (|_Suggest29450| |actions| |_Sense29466|)
              (|_Suggest29450| |actions| |_Transmit29465|)
              (|_Suggest29450| |actions| |_Embody29464|)
              (|_Suggest29450| |actions| |_Express29459|)
              (|_Suggest29450| |all-subevents| |_Sense29466|)
              (|_Suggest29450| |all-subevents| |_Transmit29465|)
              (|_Suggest29450| |all-subevents| |_Embody29464|)
              (|_Suggest29450| |all-subevents| |_Interpret29461|)
              (|_Suggest29450| |all-subevents| |_Convey29460|)
              (|_Suggest29450| |all-subevents| |_Express29459|)
              (|_Suggest29450| |preparatory-event| |_Move29467|)
              (|_Suggest29450| |primitive-actions| |_Interpret29461|)
              (|_Suggest29450| |primitive-actions| |_Sense29466|)
              (|_Suggest29450| |primitive-actions| |_Transmit29465|)
              (|_Suggest29450| |primitive-actions| |_Embody29464|)
              (|_Suggest29450| |primitive-actions| |_Express29459|)
              (|_Suggest29450| |time-during| |_Time-Interval29463|)
              (|_Suggest29450| |base| |_Message29462|)
              (|_Suggest29450| |object| |_Suggestion29457|)
              (|_Suggest29450| |recipient| |_Tangible-Entity29455|)
              (|_Suggest29450| |first-subevent| |_Express29459|)
              (|_Suggest29450| |subevent| |_Interpret29461|)
              (|_Suggest29450| |subevent| |_Convey29460|)
              (|_Suggest29450| |subevent| |_Express29459|)
              (|_Suggest29450| |resulting-state| |_Be-Known29458|))
             T NIL)))
(|UoM-Intensity| (|_UoM-Intensity37462|
                  (((|_UoM-Intensity37462| |instance-of|
                     |UoM-Intensity|))
                   T NIL)))
(|Ruin| (|_Ruin24888|
         (((|_Time-Interval24902| |instance-of| |Time-Interval|)
           (|_Tangible-Entity24898| |instance-of| |Physical-Object|)
           (|_Move24896| |instance-of| |Move|)
           (|_Time-Interval24895| |instance-of| |Time-Interval|)
           (|_Tangible-Entity24892| |instance-of| |Physical-Object|)
           (|_Ruin24888| |instance-of| |Ruin|)
           (|_Be-Ruined24894| |instance-of| |Be-Ruined|)
           (|_Be-Ruined24894| |actions| |_Be-Ruined24894|)
           (|_Be-Ruined24894| |primitive-actions| |_Be-Ruined24894|)
           (|_Be-Ruined24894| |time-during| |_Time-Interval24902|)
           (|_Be-Ruined24894| |object| |_Tangible-Entity24898|)
           (|_Ruin24888| |actions| |_Ruin24888|)
           (|_Ruin24888| |preparatory-event| |_Move24896|)
           (|_Ruin24888| |primitive-actions| |_Ruin24888|)
           (|_Ruin24888| |time-during| |_Time-Interval24895|)
           (|_Ruin24888| |object| |_Tangible-Entity24892|)
           (|_Ruin24888| |resulting-state| |_Be-Ruined24894|))
          T NIL)))
(|Place| (|_Place34671|
          (((|_Place34671| |instance-of| |Place|)) T NIL)))
(|Feather| (|_Feather34612|
            (((|_Feather34612| |instance-of| |Feather|)) T NIL)))
(|Velocity-Vector-Value| (|_Velocity-Vector-Value37785|
                          (((|_Angle-Value37787| |instance-of|
                             |Angle-Value|)
                            (|_Speed-Value37786| |instance-of|
                             |Speed-Value|)
                            (|_Speed-Value37789| |instance-of|
                             |Speed-Value|)
                            (|_Velocity-Vector-Value37785|
                             |instance-of| |Velocity-Vector-Value|)
                            (|_Speed-Value37788| |instance-of|
                             |Speed-Value|)
                            (|_Velocity-Vector-Value37785|
                             |x-component-slot| |x-speed|)
                            (|_Velocity-Vector-Value37785|
                             |y-component-slot| |y-speed|)
                            (|_Velocity-Vector-Value37785| |direction|
                             |_Angle-Value37787|)
                            (|_Velocity-Vector-Value37785| |speed|
                             |_Speed-Value37786|)
                            (|_Velocity-Vector-Value37785| |x-speed|
                             |_Speed-Value37789|)
                            (|_Velocity-Vector-Value37785| |y-speed|
                             |_Speed-Value37788|))
                           T NIL)))
(|Slope-Constant| (|_Slope-Constant37829|
                   (((|_Slope-Constant37829| |instance-of|
                      |Slope-Constant|))
                    T NIL)))
(|Compute-Qualitative-Maximum| (|_Compute-Qualitative-Maximum34958|
                                (((|_Compute-Qualitative-Maximum34958|
                                   |instance-of|
                                   |Compute-Qualitative-Maximum|))
                                 T
                                 NIL)))
(|Polarity-Constant| (|_Polarity-Constant37855|
                      (((|_Polarity-Constant37855| |instance-of|
                         |Polarity-Constant|))
                       T NIL)))
(|Temperature-Constant| (|_Temperature-Constant37821|
                         (((|_Temperature-Constant37821| |instance-of|
                            |Temperature-Constant|))
                          T NIL)))
(|Construction| (|_Construction938|
                 (((|_Construction938| |instance-of| |Construction|)
                   (|_Time-Interval944| |instance-of| |Time-Interval|)
                   (|_Construction938| |actions| |_Construction938|)
                   (|_Construction938| |primitive-actions|
                    |_Construction938|)
                   (|_Construction938| |time-during|
                    |_Time-Interval944|))
                  T NIL)))
(|Importance-Constant| (|_Importance-Constant37881|
                        (((|_Importance-Constant37881| |instance-of|
                           |Importance-Constant|))
                         T NIL)))
(|Polarity-Value| (|_Polarity-Value37714|
                   (((|_Polarity-Value37714| |instance-of|
                      |Polarity-Value|))
                    T NIL)))
(|Match-Result-Viewpoint| (|_Match-Result-Viewpoint34932|
                           (((|_Match-Result-Viewpoint34932|
                              |instance-of| |Match-Result-Viewpoint|))
                            T NIL)))
(|Damage| (|_Damage24795|
           (((|_Time-Interval24809| |instance-of| |Time-Interval|)
             (|_Tangible-Entity24805| |instance-of| |Tangible-Entity|)
             (|_Move24803| |instance-of| |Move|)
             (|_Time-Interval24802| |instance-of| |Time-Interval|)
             (|_Tangible-Entity24799| |instance-of| |Tangible-Entity|)
             (|_Damage24795| |instance-of| |Damage|)
             (|_Be-Damaged24801| |instance-of| |Be-Damaged|)
             (|_Be-Damaged24801| |actions| |_Be-Damaged24801|)
             (|_Be-Damaged24801| |primitive-actions|
              |_Be-Damaged24801|)
             (|_Be-Damaged24801| |time-during| |_Time-Interval24809|)
             (|_Be-Damaged24801| |object| |_Tangible-Entity24805|)
             (|_Damage24795| |actions| |_Damage24795|)
             (|_Damage24795| |preparatory-event| |_Move24803|)
             (|_Damage24795| |primitive-actions| |_Damage24795|)
             (|_Damage24795| |time-during| |_Time-Interval24802|)
             (|_Damage24795| |object| |_Tangible-Entity24799|)
             (|_Damage24795| |resulting-state| |_Be-Damaged24801|))
            T NIL)))
(|Size-Constant| (|_Size-Constant37831|
                  (((|_Size-Constant37831| |instance-of|
                     |Size-Constant|))
                   T NIL)))
(|Integrity-Value| (|_Integrity-Value37741|
                    (((|_Integrity-Value37741| |instance-of|
                       |Integrity-Value|))
                     T NIL)))
(|Mass-Scale| (|_Mass-Scale37562|
               (((|_Mass-Scale37562| |instance-of| |Mass-Scale|)
                 (|_Number37564| |instance-of| |Number|)
                 (|_Mass-Scale37562| |number-of-elements|
                  |_Number37564|))
                T NIL)))
(|Air| (|_Air33952|
        (((|_Categorical33957| |instance-of| |Categorical|)
          (|_Air33952| |instance-of| |Air|)
          (|_State-Value33956| |instance-of| |State-Value|)
          (|_State-Value33956| |categorical-value| |_Categorical33957|)
          (|_Air33952| |physical-state| |_State-Value33956|))
         T NIL)))
(|Trigger-Node| (|_Trigger-Node34910|
                 (((|_Trigger-Node34910| |instance-of| |Trigger-Node|))
                  T NIL)))
(|Piece-of-Glass| (|_Piece-of-Glass34123|
                   (((|_Piece-of-Glass34123| |instance-of|
                      |Piece-of-Glass|)
                     (|_Glass34126| |instance-of| |Glass|)
                     (|_Piece-of-Glass34123| |material| |_Glass34126|))
                    T NIL)))
(|Entity| (|_Entity33602|
           (((|_Entity33602| |instance-of| |Entity|)) T NIL)))
(|Community| (|_Community34887|
              (((|_Community34887| |instance-of| |Community|)
                (|_Number34889| |instance-of| |Number|)
                (|_Community34887| |number-of-elements|
                 |_Number34889|))
               T NIL)))
(|Lose| (|_Lose1242|
         (((|_Move1259| |instance-of| |Move|)
           (|_Obtain1258| |instance-of| |Obtain|)
           (|_Time-Interval1257| |instance-of| |Time-Interval|)
           (|_Move1252| |instance-of| |Move|)
           (|_Obtain1251| |instance-of| |Obtain|)
           (|_Time-Interval1250| |instance-of| |Time-Interval|)
           (|_Entity1249| |instance-of| |Spatial-Entity|)
           (|_Lose1242| |instance-of| |Lose|)
           (|_Tangible-Entity1247| |instance-of| |Tangible-Entity|)
           (|_Obtain1251| |actions| |_Obtain1251|)
           (|_Obtain1251| |preparatory-event| |_Move1259|)
           (|_Obtain1251| |preparatory-event| |_Obtain1258|)
           (|_Obtain1251| |primitive-actions| |_Obtain1251|)
           (|_Obtain1251| |time-during| |_Time-Interval1257|)
           (|_Obtain1251| |object| |_Entity1249|)
           (|_Obtain1251| |recipient| |_Tangible-Entity1247|)
           (|_Obtain1251| |agent| |_Tangible-Entity1247|)
           (|_Lose1242| |actions| |_Lose1242|)
           (|_Lose1242| |preparatory-event| |_Move1252|)
           (|_Lose1242| |preparatory-event| |_Obtain1251|)
           (|_Lose1242| |primitive-actions| |_Lose1242|)
           (|_Lose1242| |time-during| |_Time-Interval1250|)
           (|_Lose1242| |object| |_Entity1249|)
           (|_Lose1242| |donor| |_Tangible-Entity1247|))
          T NIL)))
(|Be-Accessible| (|_Be-Accessible379|
                  (((|_Time-Interval383| |instance-of| |Time-Interval|)
                    (|_Be-Accessible379| |instance-of| |Be-Accessible|)
                    (|_Entity382| |instance-of| |Entity|)
                    (|_Be-Accessible379| |actions| |_Be-Accessible379|)
                    (|_Be-Accessible379| |primitive-actions|
                     |_Be-Accessible379|)
                    (|_Be-Accessible379| |time-during|
                     |_Time-Interval383|)
                    (|_Be-Accessible379| |object| |_Entity382|))
                   T NIL)))
(|Power-Value| (|_Power-Value37712|
                (((|_Power-Value37712| |instance-of| |Power-Value|)) T
                 NIL)))
(|Equation-Big-Node| (|_Equation-Big-Node34906|
                      (((|_Equation-Big-Node34906| |instance-of|
                         |Equation-Big-Node|))
                       T NIL)))
(|Distribution| (|_Distribution857|
                 (((|_Distribution857| |instance-of| |Distribution|)
                   (|_Time-Interval863| |instance-of| |Time-Interval|)
                   (|_Distribution857| |actions| |_Distribution857|)
                   (|_Distribution857| |primitive-actions|
                    |_Distribution857|)
                   (|_Distribution857| |time-during|
                    |_Time-Interval863|))
                  T NIL)))
(|Time-Constant| (|_Time-Constant37931|
                  (((|_Time-Constant37931| |instance-of|
                     |Time-Constant|))
                   T NIL)))
(|Be-Blocked| (|_Be-Blocked313|
               (((|_Time-Interval315| |instance-of| |Time-Interval|)
                 (|_Be-Blocked313| |instance-of| |Be-Blocked|)
                 (|_Spatial-Entity314| |instance-of| |Spatial-Entity|)
                 (|_Be-Blocked313| |actions| |_Be-Blocked313|)
                 (|_Be-Blocked313| |primitive-actions|
                  |_Be-Blocked313|)
                 (|_Be-Blocked313| |time-during| |_Time-Interval315|)
                 (|_Be-Blocked313| |object| |_Spatial-Entity314|))
                T NIL)))
(|Copy| (|_Copy25789|
         (((|_Move25798| |instance-of| |Move|)
           (|_Time-Interval25797| |instance-of| |Time-Interval|)
           (|_Tangible-Entity25794| |instance-of| |Tangible-Entity|)
           (|_Copy25789| |instance-of| |Copy|)
           (|_Entity25796| |instance-of| |Entity|)
           (|_Copy25789| |actions| |_Copy25789|)
           (|_Copy25789| |preparatory-event| |_Move25798|)
           (|_Copy25789| |primitive-actions| |_Copy25789|)
           (|_Copy25789| |time-during| |_Time-Interval25797|)
           (|_Copy25789| |object| |_Tangible-Entity25794|)
           (|_Copy25789| |result| |_Entity25796|))
          T NIL)))
(|Compute-Minimum| (|_Compute-Minimum34960|
                    (((|_Compute-Minimum34960| |instance-of|
                       |Compute-Minimum|))
                     T NIL)))
(|Relation| (|_Relation37939|
             (((|_Relation37939| |instance-of| |Relation|)) T NIL)))
(|Slot| (|_Slot37937| (((|_Slot37937| |instance-of| |Slot|)) T NIL)))
(|Container| (|_Container33217|
              (((|_Time-Interval33289| |instance-of| |Time-Interval|)
                (|_Spatial-Entity33285| |instance-of|
                 |Tangible-Entity|)
                (|_Move33283| |instance-of| |Move|)
                (|_Unblock33281| |instance-of| |Unblock|)
                (|_Time-Interval33280| |instance-of| |Time-Interval|)
                (|_Be-Blocked33279| |instance-of| |Be-Blocked|)
                (|_Move33264| |instance-of| |Move|)
                (|_Block33262| |instance-of| |Block|)
                (|_Time-Interval33261| |instance-of| |Time-Interval|)
                (|_Time-Interval33254| |instance-of| |Time-Interval|)
                (|_Spatial-Entity33247| |instance-of|
                 |Tangible-Entity|)
                (|_Move33244| |instance-of| |Move|)
                (|_Unblock33242| |instance-of| |Unblock|)
                (|_Time-Interval33241| |instance-of| |Time-Interval|)
                (|_Spatial-Entity33233| |instance-of| |Spatial-Entity|)
                (|_Spatial-Entity33232| |instance-of| |Spatial-Entity|)
                (|_Tangible-Entity33240| |instance-of|
                 |Tangible-Entity|)
                (|_Tangible-Entity33236| |instance-of|
                 |Tangible-Entity|)
                (|_Be-Blocked33239| |instance-of| |Be-Blocked|)
                (|_Categorical33221| |instance-of| |Categorical|)
                (|_Spatial-Entity33224| |instance-of| |Spatial-Entity|)
                (|_Spatial-Entity33223| |instance-of| |Spatial-Entity|)
                (|_Shape-Value33220| |instance-of| |Shape-Value|)
                (|_Be-Contained33219| |instance-of| |Be-Contained|)
                (|_Be-Contained33219| |instance-of| |Block|)
                (|_Container33217| |instance-of| |Container|)
                (|_Tangible-Entity33218| |instance-of|
                 |Tangible-Entity|)
                (|_Be-Blocked33279| |object| |_Tangible-Entity33236|)
                (|_Unblock33242| |defeats| |_Be-Blocked33279|)
                (|_Be-Blocked33279| |actions| |_Be-Blocked33279|)
                (|_Be-Blocked33279| |primitive-actions|
                 |_Be-Blocked33279|)
                (|_Be-Blocked33279| |time-during|
                 |_Time-Interval33289|)
                (|_Be-Blocked33279| |object| |_Spatial-Entity33285|)
                (|_Block33262| |actions| |_Block33262|)
                (|_Block33262| |preparatory-event| |_Move33283|)
                (|_Block33262| |preparatory-event| |_Unblock33281|)
                (|_Block33262| |primitive-actions| |_Block33262|)
                (|_Block33262| |time-during| |_Time-Interval33280|)
                (|_Block33262| |base| |_Tangible-Entity33240|)
                (|_Block33262| |object| |_Tangible-Entity33236|)
                (|_Block33262| |resulting-state| |_Be-Blocked33279|)
                (|_Be-Blocked33239| |object| |_Tangible-Entity33236|)
                (|_Unblock33242| |actions| |_Unblock33242|)
                (|_Unblock33242| |preparatory-event| |_Move33264|)
                (|_Unblock33242| |preparatory-event| |_Block33262|)
                (|_Unblock33242| |primitive-actions| |_Unblock33242|)
                (|_Unblock33242| |time-during| |_Time-Interval33261|)
                (|_Unblock33242| |base| |_Tangible-Entity33240|)
                (|_Unblock33242| |object| |_Tangible-Entity33236|)
                (|_Unblock33242| |defeats| |_Be-Blocked33239|)
                (|_Be-Blocked33239| |actions| |_Be-Blocked33239|)
                (|_Be-Blocked33239| |primitive-actions|
                 |_Be-Blocked33239|)
                (|_Be-Blocked33239| |time-during|
                 |_Time-Interval33254|)
                (|_Be-Blocked33239| |object| |_Spatial-Entity33247|)
                (|_Be-Contained33219| |actions| |_Be-Contained33219|)
                (|_Be-Contained33219| |preparatory-event| |_Move33244|)
                (|_Be-Contained33219| |preparatory-event|
                 |_Unblock33242|)
                (|_Be-Contained33219| |primitive-actions|
                 |_Be-Contained33219|)
                (|_Be-Contained33219| |time-during|
                 |_Time-Interval33241|)
                (|_Be-Contained33219| |destination|
                 |_Spatial-Entity33233|)
                (|_Be-Contained33219| |origin| |_Spatial-Entity33232|)
                (|_Be-Contained33219| |base| |_Tangible-Entity33240|)
                (|_Be-Contained33219| |object| |_Tangible-Entity33236|)
                (|_Be-Contained33219| |resulting-state|
                 |_Be-Blocked33239|)
                (|_Shape-Value33220| |categorical-value|
                 |_Categorical33221|)
                (|_Tangible-Entity33218| |is-between|
                 |_Spatial-Entity33223|)
                (|_Tangible-Entity33218| |is-between|
                 |_Spatial-Entity33224|)
                (|_Tangible-Entity33218| |encloses|
                 |_Spatial-Entity33224|)
                (|_Tangible-Entity33218| |does-not-enclose|
                 |_Spatial-Entity33223|)
                (|_Tangible-Entity33218| |instrument-of|
                 |_Be-Contained33219|)
                (|_Tangible-Entity33218| |shape| |_Shape-Value33220|)
                (|_Container33217| |in-event| |_Be-Contained33219|)
                (|_Container33217| |played-by|
                 |_Tangible-Entity33218|))
               T NIL)))
(|Let-Go-Of| (|_Let-Go-Of24242|
              (((|_Time-Interval24262| |instance-of| |Time-Interval|)
                (|_Entity24258| |instance-of| |Entity|)
                (|_Move24256| |instance-of| |Move|)
                (|_Make-Accessible24255| |instance-of|
                 |Make-Accessible|)
                (|_Time-Interval24254| |instance-of| |Time-Interval|)
                (|_Be-Inaccessible24253| |instance-of|
                 |Be-Inaccessible|)
                (|_Move24248| |instance-of| |Move|)
                (|_Make-Inaccessible24247| |instance-of|
                 |Make-Inaccessible|)
                (|_Time-Interval24246| |instance-of| |Time-Interval|)
                (|_Tangible-Entity24245| |instance-of|
                 |Tangible-Entity|)
                (|_Let-Go-Of24242| |instance-of| |Let-Go-Of|)
                (|_Tangible-Entity24243| |instance-of|
                 |Tangible-Entity|)
                (|_Be-Inaccessible24253| |actions|
                 |_Be-Inaccessible24253|)
                (|_Be-Inaccessible24253| |primitive-actions|
                 |_Be-Inaccessible24253|)
                (|_Be-Inaccessible24253| |time-during|
                 |_Time-Interval24262|)
                (|_Be-Inaccessible24253| |object| |_Entity24258|)
                (|_Make-Inaccessible24247| |actions|
                 |_Make-Inaccessible24247|)
                (|_Make-Inaccessible24247| |preparatory-event|
                 |_Move24256|)
                (|_Make-Inaccessible24247| |preparatory-event|
                 |_Make-Accessible24255|)
                (|_Make-Inaccessible24247| |primitive-actions|
                 |_Make-Inaccessible24247|)
                (|_Make-Inaccessible24247| |time-during|
                 |_Time-Interval24254|)
                (|_Make-Inaccessible24247| |object|
                 |_Tangible-Entity24245|)
                (|_Make-Inaccessible24247| |resulting-state|
                 |_Be-Inaccessible24253|)
                (|_Make-Inaccessible24247| |agent|
                 |_Tangible-Entity24243|)
                (|_Let-Go-Of24242| |actions| |_Let-Go-Of24242|)
                (|_Let-Go-Of24242| |preparatory-event| |_Move24248|)
                (|_Let-Go-Of24242| |preparatory-event|
                 |_Make-Inaccessible24247|)
                (|_Let-Go-Of24242| |primitive-actions|
                 |_Let-Go-Of24242|)
                (|_Let-Go-Of24242| |time-during| |_Time-Interval24246|)
                (|_Let-Go-Of24242| |object| |_Tangible-Entity24245|)
                (|_Let-Go-Of24242| |agent| |_Tangible-Entity24243|))
               T NIL)))
(|Circumference-Constant| (|_Circumference-Constant37907|
                           (((|_Circumference-Constant37907|
                              |instance-of| |Circumference-Constant|))
                            T NIL)))
(|Sex-Value| (|_Sex-Value37701|
              (((|_Sex-Value37701| |instance-of| |Sex-Value|)) T NIL)))
(|Be-Obstructed| (|_Be-Obstructed257|
                  (((|_Time-Interval261| |instance-of| |Time-Interval|)
                    (|_Be-Obstructed257| |instance-of| |Be-Obstructed|)
                    (|_Entity260| |instance-of| |Entity|)
                    (|_Be-Obstructed257| |actions| |_Be-Obstructed257|)
                    (|_Be-Obstructed257| |primitive-actions|
                     |_Be-Obstructed257|)
                    (|_Be-Obstructed257| |time-during|
                     |_Time-Interval261|)
                    (|_Be-Obstructed257| |object| |_Entity260|))
                   T NIL)))
(|UoM-Moment-of-Inertia| (|_UoM-Moment-of-Inertia37450|
                          (((|_UoM-Moment-of-Inertia37450|
                             |instance-of| |UoM-Moment-of-Inertia|))
                           T NIL)))
(|Be-Attached-To| (|_Be-Attached-To45|
                   (((|_Be-Attached-To45| |instance-of|
                      |Be-Attached-To|)
                     (|_Time-Interval47| |instance-of| |Time-Interval|)
                     (|_Be-Attached-To45| |actions|
                      |_Be-Attached-To45|)
                     (|_Be-Attached-To45| |primitive-actions|
                      |_Be-Attached-To45|)
                     (|_Be-Attached-To45| |time-during|
                      |_Time-Interval47|))
                    T NIL)))
(|Request| (|_Request34803|
            (((|_Request34803| |instance-of| |Request|)
              (|_Thing34804| |instance-of| |Thing|)
              (|_Request34803| |information-content| |_Thing34804|))
             T NIL)))
(|Mark| (|_Mark25635|
         (((|_Move25648| |instance-of| |Move|)
           (|_Time-Interval25647| |instance-of| |Time-Interval|)
           (|_Message25645| |instance-of| |Message|)
           (|_Message25645| |instance-of| |Spatial-Entity|)
           (|_Tangible-Entity25642| |instance-of| |Tangible-Entity|)
           (|_Mark25635| |instance-of| |Mark|)
           (|_Physical-Mark25646| |instance-of| |Physical-Mark|)
           (|_Mark25635| |actions| |_Mark25635|)
           (|_Mark25635| |preparatory-event| |_Move25648|)
           (|_Mark25635| |primitive-actions| |_Mark25635|)
           (|_Mark25635| |time-during| |_Time-Interval25647|)
           (|_Mark25635| |object| |_Message25645|)
           (|_Mark25635| |raw-material| |_Tangible-Entity25642|)
           (|_Mark25635| |result| |_Physical-Mark25646|))
          T NIL)))
(|Priority-Constant| (|_Priority-Constant37847|
                      (((|_Priority-Constant37847| |instance-of|
                         |Priority-Constant|))
                       T NIL)))
(|Be-Available| (|_Be-Available372|
                 (((|_Time-Interval376| |instance-of| |Time-Interval|)
                   (|_Be-Available372| |instance-of| |Be-Available|)
                   (|_Resource375| |instance-of| |Resource|)
                   (|_Be-Available372| |actions| |_Be-Available372|)
                   (|_Be-Available372| |primitive-actions|
                    |_Be-Available372|)
                   (|_Be-Available372| |time-during|
                    |_Time-Interval376|)
                   (|_Be-Available372| |object| |_Resource375|))
                  T NIL)))
(|Planning| (|_Planning507|
             (((|_Planning507| |instance-of| |Planning|)
               (|_Time-Interval513| |instance-of| |Time-Interval|)
               (|_Planning507| |actions| |_Planning507|)
               (|_Planning507| |primitive-actions| |_Planning507|)
               (|_Planning507| |time-during| |_Time-Interval513|))
              T NIL)))
(|Time-Instant| (|_Time-Instant34896|
                 (((|_Time-Instant34896| |instance-of| |Time-Instant|)
                   (|_Time-Instant34896| |time-begins|
                    |_Time-Instant34896|)
                   (|_Time-Instant34896| |time-ends|
                    |_Time-Instant34896|))
                  T NIL)))
(|Take-Apart| (|_Take-Apart24856|
               (((|_Time-Interval24870| |instance-of| |Time-Interval|)
                 (|_Physical-Object24866| |instance-of|
                  |Physical-Object|)
                 (|_Move24864| |instance-of| |Move|)
                 (|_Time-Interval24863| |instance-of| |Time-Interval|)
                 (|_Tangible-Entity24860| |instance-of|
                  |Physical-Object|)
                 (|_Take-Apart24856| |instance-of| |Take-Apart|)
                 (|_Be-Broken24862| |instance-of| |Be-Broken|)
                 (|_Be-Broken24862| |actions| |_Be-Broken24862|)
                 (|_Be-Broken24862| |primitive-actions|
                  |_Be-Broken24862|)
                 (|_Be-Broken24862| |time-during|
                  |_Time-Interval24870|)
                 (|_Be-Broken24862| |object| |_Physical-Object24866|)
                 (|_Take-Apart24856| |actions| |_Take-Apart24856|)
                 (|_Take-Apart24856| |preparatory-event| |_Move24864|)
                 (|_Take-Apart24856| |primitive-actions|
                  |_Take-Apart24856|)
                 (|_Take-Apart24856| |time-during|
                  |_Time-Interval24863|)
                 (|_Take-Apart24856| |object| |_Tangible-Entity24860|)
                 (|_Take-Apart24856| |resulting-state|
                  |_Be-Broken24862|))
                T NIL)))
(|Document| (|_Document34779|
             (((|_Language34782| |instance-of| |Language|)
               (|_Write34781| |instance-of| |Write|)
               (|_Document34779| |instance-of| |Document|)
               (|_Thing34780| |instance-of| |Thing|)
               (|_Document34779| |information-language|
                |_Language34782|)
               (|_Document34779| |result-of| |_Write34781|)
               (|_Document34779| |information-content| |_Thing34780|))
              T NIL)))
(|Be-Held| (|_Be-Held288|
            (((|_Time-Interval294| |instance-of| |Time-Interval|)
              (|_Tangible-Entity293| |instance-of| |Tangible-Entity|)
              (|_Be-Held288| |instance-of| |Be-Held|)
              (|_Tangible-Entity292| |instance-of| |Tangible-Entity|)
              (|_Be-Held288| |actions| |_Be-Held288|)
              (|_Be-Held288| |primitive-actions| |_Be-Held288|)
              (|_Be-Held288| |time-during| |_Time-Interval294|)
              (|_Be-Held288| |object| |_Tangible-Entity293|)
              (|_Be-Held288| |agent| |_Tangible-Entity292|))
             T NIL)))
(|Go-To| (|_Go-To35156|
          (((|_Leave35185| |instance-of| |Leave|)
            (|_Time-Interval35184| |instance-of| |Time-Interval|)
            (|_Spatial-Entity35158| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity35157| |instance-of| |Tangible-Entity|)
            (|_Acceleration-Magnitude-Value35161| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value35160| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value35167| |instance-of| |Length-Value|)
            (|_Duration-Value35159| |instance-of| |Duration-Value|)
            (|_Speed-Value35163| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value35166| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value35183| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value35180| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value35182| |instance-of| |Speed-Value|)
            (|_Speed-Value35181| |instance-of| |Speed-Value|)
            (|_Speed-Value35179| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value35176| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value35178| |instance-of| |Speed-Value|)
            (|_Speed-Value35177| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value35162| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value35175| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value35174| |instance-of| |Length-Value|)
            (|_Speed-Value35173| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value35172| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value35171| |instance-of| |Length-Value|)
            (|_Go-To35156| |instance-of| |Go-To|)
            (|_Speed-Value35170| |instance-of| |Speed-Value|)
            (|_Go-To35156| |actions| |_Go-To35156|)
            (|_Go-To35156| |preparatory-event| |_Leave35185|)
            (|_Go-To35156| |primitive-actions| |_Go-To35156|)
            (|_Go-To35156| |time-during| |_Time-Interval35184|)
            (|_Go-To35156| |destination| |_Spatial-Entity35158|)
            (|_Go-To35156| |object| |_Tangible-Entity35157|)
            (|_Go-To35156| |agent| |_Tangible-Entity35157|)
            (|_Go-To35156| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value35161|)
            (|_Go-To35156| |acceleration|
             |_Acceleration-Vector-Value35160|)
            (|_Go-To35156| |distance| |_Length-Value35167|)
            (|_Go-To35156| |duration| |_Duration-Value35159|)
            (|_Go-To35156| |speed| |_Speed-Value35163|)
            (|_Go-To35156| |displacement|
             |_Displacement-Vector-Value35166|)
            (|_Go-To35156| |final-speed| |_Speed-Value35183|)
            (|_Go-To35156| |final-velocity|
             |_Velocity-Vector-Value35180|)
            (|_Go-To35156| |final-x-speed| |_Speed-Value35182|)
            (|_Go-To35156| |final-y-speed| |_Speed-Value35181|)
            (|_Go-To35156| |initial-speed| |_Speed-Value35179|)
            (|_Go-To35156| |initial-velocity|
             |_Velocity-Vector-Value35176|)
            (|_Go-To35156| |initial-x-speed| |_Speed-Value35178|)
            (|_Go-To35156| |initial-y-speed| |_Speed-Value35177|)
            (|_Go-To35156| |velocity| |_Velocity-Vector-Value35162|)
            (|_Go-To35156| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value35175|)
            (|_Go-To35156| |x-distance| |_Length-Value35174|)
            (|_Go-To35156| |x-speed| |_Speed-Value35173|)
            (|_Go-To35156| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value35172|)
            (|_Go-To35156| |y-distance| |_Length-Value35171|)
            (|_Go-To35156| |y-speed| |_Speed-Value35170|))
           T NIL)))
(|Agentive-Relation| (|_Agentive-Relation37951|
                      (((|_Agentive-Relation37951| |instance-of|
                         |Agentive-Relation|))
                       T NIL)))
(|Time-Zone-Constant| (|_Time-Zone-Constant37815|
                       (((|_Time-Zone-Constant37815| |instance-of|
                          |Time-Zone-Constant|))
                        T NIL)))
(|Identification-Constant| (|_Identification-Constant37883|
                            (((|_Identification-Constant37883|
                               |instance-of|
                               |Identification-Constant|))
                             T NIL)))
(|Heat| (|_Heat24681|
         (((|_Time-Interval24692| |instance-of| |Time-Interval|)
           (|_Tangible-Entity24691| |instance-of| |Tangible-Entity|)
           (|_Temperature-Value24687| |instance-of|
            |Temperature-Value|)
           (|_Heat24681| |instance-of| |Heat|)
           (|_Temperature-Value24688| |instance-of|
            |Temperature-Value|)
           (|_Temperature-Value24688| |greater-than|
            |_Temperature-Value24687|)
           (|_Heat24681| |actions| |_Heat24681|)
           (|_Heat24681| |primitive-actions| |_Heat24681|)
           (|_Heat24681| |time-during| |_Time-Interval24692|)
           (|_Heat24681| |base| |_Tangible-Entity24691|)
           (|_Heat24681| |from-value| |_Temperature-Value24687|)
           (|_Heat24681| |to-value| |_Temperature-Value24688|))
          T NIL)))
(|Seat| (|_Seat32959| (((|_Seat32959| |instance-of| |Seat|)) T NIL)))
(|Physical-Document| (|_Physical-Document34153|
                      (((|_Print34155| |instance-of| |Print|)
                        (|_Physical-Document34153| |instance-of|
                         |Physical-Document|)
                        (|_Physical-Mark34154| |instance-of|
                         |Physical-Mark|)
                        (|_Physical-Document34153| |result-of|
                         |_Print34155|)
                        (|_Physical-Document34153| |has-part|
                         |_Physical-Mark34154|))
                       T NIL)))
(|Select| (|_Select2321|
           (((|_Move2328| |instance-of| |Move|)
             (|_Time-Interval2327| |instance-of| |Time-Interval|)
             (|_Select2321| |instance-of| |Select|)
             (|_Entity2325| |instance-of| |Spatial-Entity|)
             (|_Select2321| |actions| |_Select2321|)
             (|_Select2321| |preparatory-event| |_Move2328|)
             (|_Select2321| |primitive-actions| |_Select2321|)
             (|_Select2321| |time-during| |_Time-Interval2327|)
             (|_Select2321| |base| |_Entity2325|)
             (|_Select2321| |object| |_Entity2325|))
            T NIL)))
(|Move-Out-Of| (|_Move-Out-Of16013|
                (((|_Move16106| |instance-of| |Move|)
                  (|_Time-Interval16105| |instance-of| |Time-Interval|)
                  (|_Tangible-Entity16104| |instance-of|
                   |Tangible-Entity|)
                  (|_Acceleration-Magnitude-Value16103| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Acceleration-Vector-Value16087| |instance-of|
                   |Acceleration-Vector-Value|)
                  (|_Length-Value16102| |instance-of| |Length-Value|)
                  (|_Duration-Value16101| |instance-of|
                   |Duration-Value|)
                  (|_Speed-Value16100| |instance-of| |Speed-Value|)
                  (|_Displacement-Vector-Value16085| |instance-of|
                   |Displacement-Vector-Value|)
                  (|_Speed-Value16099| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16096| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Speed-Value16098| |instance-of| |Speed-Value|)
                  (|_Speed-Value16097| |instance-of| |Speed-Value|)
                  (|_Speed-Value16095| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16092| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Speed-Value16094| |instance-of| |Speed-Value|)
                  (|_Speed-Value16093| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16083| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Acceleration-Magnitude-Value16091| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Length-Value16090| |instance-of| |Length-Value|)
                  (|_Speed-Value16089| |instance-of| |Speed-Value|)
                  (|_Acceleration-Magnitude-Value16088| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Length-Value16086| |instance-of| |Length-Value|)
                  (|_Speed-Value16084| |instance-of| |Speed-Value|)
                  (|_Move16082| |instance-of| |Move|)
                  (|_Time-Interval16081| |instance-of| |Time-Interval|)
                  (|_Acceleration-Magnitude-Value16080| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Acceleration-Vector-Value16064| |instance-of|
                   |Acceleration-Vector-Value|)
                  (|_Length-Value16079| |instance-of| |Length-Value|)
                  (|_Duration-Value16078| |instance-of|
                   |Duration-Value|)
                  (|_Speed-Value16077| |instance-of| |Speed-Value|)
                  (|_Displacement-Vector-Value16062| |instance-of|
                   |Displacement-Vector-Value|)
                  (|_Speed-Value16076| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16073| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Speed-Value16075| |instance-of| |Speed-Value|)
                  (|_Speed-Value16074| |instance-of| |Speed-Value|)
                  (|_Speed-Value16072| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16069| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Speed-Value16071| |instance-of| |Speed-Value|)
                  (|_Speed-Value16070| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16060| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Acceleration-Magnitude-Value16068| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Length-Value16067| |instance-of| |Length-Value|)
                  (|_Speed-Value16066| |instance-of| |Speed-Value|)
                  (|_Acceleration-Magnitude-Value16065| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Length-Value16063| |instance-of| |Length-Value|)
                  (|_Speed-Value16061| |instance-of| |Speed-Value|)
                  (|_Move16059| |instance-of| |Move|)
                  (|_Admit16057| |instance-of| |Admit|)
                  (|_Time-Interval16056| |instance-of| |Time-Interval|)
                  (|_Be-Shut-Out16055| |instance-of| |Be-Shut-Out|)
                  (|_Portal16054| |instance-of| |Portal|)
                  (|_Barrier16053| |instance-of| |Container|)
                  (|_Angle-Value16052| |instance-of| |Angle-Value|)
                  (|_Angle-Value16051| |instance-of| |Angle-Value|)
                  (|_Angle-Value16050| |instance-of| |Angle-Value|)
                  (|_Angle-Value16049| |instance-of| |Angle-Value|)
                  (|_Angle-Value16048| |instance-of| |Angle-Value|)
                  (|_Move16044| |instance-of| |Move|)
                  (|_Move16043| |instance-of| |Move|)
                  (|_Shut-Out16042| |instance-of| |Shut-Out|)
                  (|_Time-Interval16041| |instance-of| |Time-Interval|)
                  (|_Spatial-Entity16037| |instance-of|
                   |Spatial-Entity|)
                  (|_Spatial-Entity16036| |instance-of|
                   |Spatial-Entity|)
                  (|_Spatial-Entity16038| |instance-of|
                   |Spatial-Entity|)
                  (|_Tangible-Entity16039| |instance-of|
                   |Tangible-Entity|)
                  (|_Tangible-Entity16035| |instance-of|
                   |Tangible-Entity|)
                  (|_Acceleration-Magnitude-Value16034| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Acceleration-Vector-Value16018| |instance-of|
                   |Acceleration-Vector-Value|)
                  (|_Length-Value16033| |instance-of| |Length-Value|)
                  (|_Duration-Value16032| |instance-of|
                   |Duration-Value|)
                  (|_Speed-Value16031| |instance-of| |Speed-Value|)
                  (|_Displacement-Vector-Value16016| |instance-of|
                   |Displacement-Vector-Value|)
                  (|_Speed-Value16030| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16027| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Speed-Value16029| |instance-of| |Speed-Value|)
                  (|_Speed-Value16028| |instance-of| |Speed-Value|)
                  (|_Speed-Value16026| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16023| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Speed-Value16025| |instance-of| |Speed-Value|)
                  (|_Speed-Value16024| |instance-of| |Speed-Value|)
                  (|_Velocity-Vector-Value16014| |instance-of|
                   |Velocity-Vector-Value|)
                  (|_Acceleration-Magnitude-Value16022| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Length-Value16021| |instance-of| |Length-Value|)
                  (|_Speed-Value16020| |instance-of| |Speed-Value|)
                  (|_Acceleration-Magnitude-Value16019| |instance-of|
                   |Acceleration-Magnitude-Value|)
                  (|_Length-Value16017| |instance-of| |Length-Value|)
                  (|_Move-Out-Of16013| |instance-of| |Move-Out-Of|)
                  (|_Speed-Value16015| |instance-of| |Speed-Value|)
                  (|_Move16044| |actions| |_Move16044|)
                  (|_Move16044| |preparatory-event| |_Move16106|)
                  (|_Move16044| |primitive-actions| |_Move16044|)
                  (|_Move16044| |time-during| |_Time-Interval16105|)
                  (|_Move16044| |object| |_Tangible-Entity16104|)
                  (|_Move16044| |acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16103|)
                  (|_Move16044| |acceleration|
                   |_Acceleration-Vector-Value16087|)
                  (|_Move16044| |distance| |_Length-Value16102|)
                  (|_Move16044| |duration| |_Duration-Value16101|)
                  (|_Move16044| |speed| |_Speed-Value16100|)
                  (|_Move16044| |displacement|
                   |_Displacement-Vector-Value16085|)
                  (|_Move16044| |final-speed| |_Speed-Value16099|)
                  (|_Move16044| |final-velocity|
                   |_Velocity-Vector-Value16096|)
                  (|_Move16044| |final-x-speed| |_Speed-Value16098|)
                  (|_Move16044| |final-y-speed| |_Speed-Value16097|)
                  (|_Move16044| |initial-speed| |_Speed-Value16095|)
                  (|_Move16044| |initial-velocity|
                   |_Velocity-Vector-Value16092|)
                  (|_Move16044| |initial-x-speed| |_Speed-Value16094|)
                  (|_Move16044| |initial-y-speed| |_Speed-Value16093|)
                  (|_Move16044| |velocity|
                   |_Velocity-Vector-Value16083|)
                  (|_Move16044| |x-acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16091|)
                  (|_Move16044| |x-distance| |_Length-Value16090|)
                  (|_Move16044| |x-speed| |_Speed-Value16089|)
                  (|_Move16044| |y-acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16088|)
                  (|_Move16044| |y-distance| |_Length-Value16086|)
                  (|_Move16044| |y-speed| |_Speed-Value16084|)
                  (|_Move16043| |actions| |_Move16043|)
                  (|_Move16043| |preparatory-event| |_Move16082|)
                  (|_Move16043| |primitive-actions| |_Move16043|)
                  (|_Move16043| |time-during| |_Time-Interval16081|)
                  (|_Move16043| |destination| |_Spatial-Entity16036|)
                  (|_Move16043| |object| |_Tangible-Entity16035|)
                  (|_Move16043| |acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16080|)
                  (|_Move16043| |acceleration|
                   |_Acceleration-Vector-Value16064|)
                  (|_Move16043| |distance| |_Length-Value16079|)
                  (|_Move16043| |duration| |_Duration-Value16078|)
                  (|_Move16043| |speed| |_Speed-Value16077|)
                  (|_Move16043| |displacement|
                   |_Displacement-Vector-Value16062|)
                  (|_Move16043| |final-speed| |_Speed-Value16076|)
                  (|_Move16043| |final-velocity|
                   |_Velocity-Vector-Value16073|)
                  (|_Move16043| |final-x-speed| |_Speed-Value16075|)
                  (|_Move16043| |final-y-speed| |_Speed-Value16074|)
                  (|_Move16043| |initial-speed| |_Speed-Value16072|)
                  (|_Move16043| |initial-velocity|
                   |_Velocity-Vector-Value16069|)
                  (|_Move16043| |initial-x-speed| |_Speed-Value16071|)
                  (|_Move16043| |initial-y-speed| |_Speed-Value16070|)
                  (|_Move16043| |velocity|
                   |_Velocity-Vector-Value16060|)
                  (|_Move16043| |x-acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16068|)
                  (|_Move16043| |x-distance| |_Length-Value16067|)
                  (|_Move16043| |x-speed| |_Speed-Value16066|)
                  (|_Move16043| |y-acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16065|)
                  (|_Move16043| |y-distance| |_Length-Value16063|)
                  (|_Move16043| |y-speed| |_Speed-Value16061|)
                  (|_Shut-Out16042| |actions| |_Shut-Out16042|)
                  (|_Shut-Out16042| |preparatory-event| |_Move16059|)
                  (|_Shut-Out16042| |preparatory-event| |_Admit16057|)
                  (|_Shut-Out16042| |primitive-actions|
                   |_Shut-Out16042|)
                  (|_Shut-Out16042| |time-during|
                   |_Time-Interval16056|)
                  (|_Shut-Out16042| |base| |_Tangible-Entity16039|)
                  (|_Shut-Out16042| |object| |_Tangible-Entity16035|)
                  (|_Shut-Out16042| |resulting-state|
                   |_Be-Shut-Out16055|)
                  (|_Spatial-Entity16037| |is-outside|
                   |_Tangible-Entity16039|)
                  (|_Spatial-Entity16038| |plays| |_Portal16054|)
                  (|_Tangible-Entity16039| |encloses|
                   |_Spatial-Entity16036|)
                  (|_Tangible-Entity16039| |plays| |_Barrier16053|)
                  (|_Tangible-Entity16039| |has-region|
                   |_Spatial-Entity16038|)
                  (|_Tangible-Entity16035| |destination-of|
                   |_Move16044|)
                  (|_Acceleration-Vector-Value16018| |x-component-slot|
                   |x-acceleration-magnitude|)
                  (|_Acceleration-Vector-Value16018| |y-component-slot|
                   |y-acceleration-magnitude|)
                  (|_Acceleration-Vector-Value16018|
                   |acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16034|)
                  (|_Acceleration-Vector-Value16018| |direction|
                   |_Angle-Value16052|)
                  (|_Displacement-Vector-Value16016| |x-component-slot|
                   |x-distance|)
                  (|_Displacement-Vector-Value16016| |y-component-slot|
                   |y-distance|)
                  (|_Displacement-Vector-Value16016| |direction|
                   |_Angle-Value16051|)
                  (|_Displacement-Vector-Value16016| |distance|
                   |_Length-Value16033|)
                  (|_Velocity-Vector-Value16027| |x-component-slot|
                   |x-speed|)
                  (|_Velocity-Vector-Value16027| |y-component-slot|
                   |y-speed|)
                  (|_Velocity-Vector-Value16027| |direction|
                   |_Angle-Value16050|)
                  (|_Velocity-Vector-Value16027| |speed|
                   |_Speed-Value16030|)
                  (|_Speed-Value16029| |x-speed-of|
                   |_Velocity-Vector-Value16027|)
                  (|_Speed-Value16028| |y-speed-of|
                   |_Velocity-Vector-Value16027|)
                  (|_Velocity-Vector-Value16023| |x-component-slot|
                   |x-speed|)
                  (|_Velocity-Vector-Value16023| |y-component-slot|
                   |y-speed|)
                  (|_Velocity-Vector-Value16023| |direction|
                   |_Angle-Value16049|)
                  (|_Velocity-Vector-Value16023| |speed|
                   |_Speed-Value16026|)
                  (|_Speed-Value16025| |x-speed-of|
                   |_Velocity-Vector-Value16023|)
                  (|_Speed-Value16024| |y-speed-of|
                   |_Velocity-Vector-Value16023|)
                  (|_Velocity-Vector-Value16014| |x-component-slot|
                   |x-speed|)
                  (|_Velocity-Vector-Value16014| |y-component-slot|
                   |y-speed|)
                  (|_Velocity-Vector-Value16014| |direction|
                   |_Angle-Value16048|)
                  (|_Velocity-Vector-Value16014| |speed|
                   |_Speed-Value16031|)
                  (|_Acceleration-Magnitude-Value16022|
                   |x-acceleration-magnitude-of|
                   |_Acceleration-Vector-Value16018|)
                  (|_Length-Value16021| |x-distance-of|
                   |_Displacement-Vector-Value16016|)
                  (|_Speed-Value16020| |x-speed-of|
                   |_Velocity-Vector-Value16014|)
                  (|_Acceleration-Magnitude-Value16019|
                   |y-acceleration-magnitude-of|
                   |_Acceleration-Vector-Value16018|)
                  (|_Length-Value16017| |y-distance-of|
                   |_Displacement-Vector-Value16016|)
                  (|_Speed-Value16015| |y-speed-of|
                   |_Velocity-Vector-Value16014|)
                  (|_Move-Out-Of16013| |actions| |_Move-Out-Of16013|)
                  (|_Move-Out-Of16013| |preparatory-event|
                   |_Move16044|)
                  (|_Move-Out-Of16013| |preparatory-event|
                   |_Move16043|)
                  (|_Move-Out-Of16013| |preparatory-event|
                   |_Shut-Out16042|)
                  (|_Move-Out-Of16013| |primitive-actions|
                   |_Move-Out-Of16013|)
                  (|_Move-Out-Of16013| |time-during|
                   |_Time-Interval16041|)
                  (|_Move-Out-Of16013| |destination|
                   |_Spatial-Entity16037|)
                  (|_Move-Out-Of16013| |origin| |_Spatial-Entity16036|)
                  (|_Move-Out-Of16013| |path| |_Spatial-Entity16038|)
                  (|_Move-Out-Of16013| |base| |_Tangible-Entity16039|)
                  (|_Move-Out-Of16013| |object|
                   |_Tangible-Entity16035|)
                  (|_Move-Out-Of16013| |acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16034|)
                  (|_Move-Out-Of16013| |acceleration|
                   |_Acceleration-Vector-Value16018|)
                  (|_Move-Out-Of16013| |distance| |_Length-Value16033|)
                  (|_Move-Out-Of16013| |duration|
                   |_Duration-Value16032|)
                  (|_Move-Out-Of16013| |speed| |_Speed-Value16031|)
                  (|_Move-Out-Of16013| |displacement|
                   |_Displacement-Vector-Value16016|)
                  (|_Move-Out-Of16013| |final-speed|
                   |_Speed-Value16030|)
                  (|_Move-Out-Of16013| |final-velocity|
                   |_Velocity-Vector-Value16027|)
                  (|_Move-Out-Of16013| |final-x-speed|
                   |_Speed-Value16029|)
                  (|_Move-Out-Of16013| |final-y-speed|
                   |_Speed-Value16028|)
                  (|_Move-Out-Of16013| |initial-speed|
                   |_Speed-Value16026|)
                  (|_Move-Out-Of16013| |initial-velocity|
                   |_Velocity-Vector-Value16023|)
                  (|_Move-Out-Of16013| |initial-x-speed|
                   |_Speed-Value16025|)
                  (|_Move-Out-Of16013| |initial-y-speed|
                   |_Speed-Value16024|)
                  (|_Move-Out-Of16013| |velocity|
                   |_Velocity-Vector-Value16014|)
                  (|_Move-Out-Of16013| |x-acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16022|)
                  (|_Move-Out-Of16013| |x-distance|
                   |_Length-Value16021|)
                  (|_Move-Out-Of16013| |x-speed| |_Speed-Value16020|)
                  (|_Move-Out-Of16013| |y-acceleration-magnitude|
                   |_Acceleration-Magnitude-Value16019|)
                  (|_Move-Out-Of16013| |y-distance|
                   |_Length-Value16017|)
                  (|_Move-Out-Of16013| |y-speed| |_Speed-Value16015|))
                 NIL NIL)))
(|Typical-Instance| (|_Typical-Instance34942|
                     (((|_Typical-Instance34942| |instance-of|
                        |Typical-Instance|))
                      T NIL)))
(|Width-Scale| (|_Width-Scale37497|
                (((|_Width-Scale37497| |instance-of| |Width-Scale|)
                  (|_Number37499| |instance-of| |Number|)
                  (|_Width-Scale37497| |number-of-elements|
                   |_Number37499|))
                 T NIL)))
(|Walk| (|_Walk35039|
         (((|_Time-Interval35083| |instance-of| |Time-Interval|)
           (|_Animal35054| |instance-of| |Animal|)
           (|_Acceleration-Magnitude-Value35058| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Acceleration-Vector-Value35057| |instance-of|
            |Acceleration-Vector-Value|)
           (|_Length-Value35064| |instance-of| |Length-Value|)
           (|_Duration-Value35056| |instance-of| |Duration-Value|)
           (|_Speed-Value35060| |instance-of| |Speed-Value|)
           (|_Displacement-Vector-Value35063| |instance-of|
            |Displacement-Vector-Value|)
           (|_Speed-Value35080| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value35077| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value35079| |instance-of| |Speed-Value|)
           (|_Speed-Value35078| |instance-of| |Speed-Value|)
           (|_Speed-Value35076| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value35073| |instance-of|
            |Velocity-Vector-Value|)
           (|_Speed-Value35075| |instance-of| |Speed-Value|)
           (|_Speed-Value35074| |instance-of| |Speed-Value|)
           (|_Velocity-Vector-Value35059| |instance-of|
            |Velocity-Vector-Value|)
           (|_Acceleration-Magnitude-Value35072| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value35071| |instance-of| |Length-Value|)
           (|_Speed-Value35070| |instance-of| |Speed-Value|)
           (|_Acceleration-Magnitude-Value35069| |instance-of|
            |Acceleration-Magnitude-Value|)
           (|_Length-Value35068| |instance-of| |Length-Value|)
           (|_Walk35039| |instance-of| |Walk|)
           (|_Speed-Value35067| |instance-of| |Speed-Value|)
           (|_Walk35039| |actions| |_Walk35039|)
           (|_Walk35039| |primitive-actions| |_Walk35039|)
           (|_Walk35039| |time-during| |_Time-Interval35083|)
           (|_Walk35039| |object| |_Animal35054|)
           (|_Walk35039| |agent| |_Animal35054|)
           (|_Walk35039| |acceleration-magnitude|
            |_Acceleration-Magnitude-Value35058|)
           (|_Walk35039| |acceleration|
            |_Acceleration-Vector-Value35057|)
           (|_Walk35039| |distance| |_Length-Value35064|)
           (|_Walk35039| |duration| |_Duration-Value35056|)
           (|_Walk35039| |speed| |_Speed-Value35060|)
           (|_Walk35039| |displacement|
            |_Displacement-Vector-Value35063|)
           (|_Walk35039| |final-speed| |_Speed-Value35080|)
           (|_Walk35039| |final-velocity|
            |_Velocity-Vector-Value35077|)
           (|_Walk35039| |final-x-speed| |_Speed-Value35079|)
           (|_Walk35039| |final-y-speed| |_Speed-Value35078|)
           (|_Walk35039| |initial-speed| |_Speed-Value35076|)
           (|_Walk35039| |initial-velocity|
            |_Velocity-Vector-Value35073|)
           (|_Walk35039| |initial-x-speed| |_Speed-Value35075|)
           (|_Walk35039| |initial-y-speed| |_Speed-Value35074|)
           (|_Walk35039| |velocity| |_Velocity-Vector-Value35059|)
           (|_Walk35039| |x-acceleration-magnitude|
            |_Acceleration-Magnitude-Value35072|)
           (|_Walk35039| |x-distance| |_Length-Value35071|)
           (|_Walk35039| |x-speed| |_Speed-Value35070|)
           (|_Walk35039| |y-acceleration-magnitude|
            |_Acceleration-Magnitude-Value35069|)
           (|_Walk35039| |y-distance| |_Length-Value35068|)
           (|_Walk35039| |y-speed| |_Speed-Value35067|))
          T NIL)))
(|Architectural-Structure| (|_Architectural-Structure34211|
                            (((|_Time-Interval34224| |instance-of|
                               |Time-Interval|)
                              (|_Time-Interval34221| |instance-of|
                               |Time-Interval|)
                              (|_Time-Interval34218| |instance-of|
                               |Time-Interval|)
                              (|_Be-Stable34215| |instance-of|
                               |Be-Stable|)
                              (|_Be-Supported34214| |instance-of|
                               |Be-Supported|)
                              (|_Architectural-Structure34211|
                               |instance-of| |Architectural-Structure|)
                              (|_Create34213| |instance-of| |Create|)
                              (|_Be-Stable34215| |actions|
                               |_Be-Stable34215|)
                              (|_Be-Stable34215| |primitive-actions|
                               |_Be-Stable34215|)
                              (|_Be-Stable34215| |time-during|
                               |_Time-Interval34224|)
                              (|_Be-Supported34214| |actions|
                               |_Be-Supported34214|)
                              (|_Be-Supported34214| |primitive-actions|
                               |_Be-Supported34214|)
                              (|_Be-Supported34214| |time-during|
                               |_Time-Interval34221|)
                              (|_Create34213| |actions| |_Create34213|)
                              (|_Create34213| |primitive-actions|
                               |_Create34213|)
                              (|_Create34213| |time-during|
                               |_Time-Interval34218|)
                              (|_Architectural-Structure34211|
                               |object-of| |_Be-Stable34215|)
                              (|_Architectural-Structure34211|
                               |object-of| |_Be-Supported34214|)
                              (|_Architectural-Structure34211|
                               |result-of| |_Create34213|))
                             T NIL)))
(|Smell-Value| (|_Smell-Value37696|
                (((|_Smell-Value37696| |instance-of| |Smell-Value|)) T
                 NIL)))
(|Eat| (|_Eat20325|
        (((|_Container20435| |instance-of| |Container|)
          (|_Time-Interval20434| |instance-of| |Time-Interval|)
          (|_Tangible-Entity20428| |instance-of| |Tangible-Entity|)
          (|_Tangible-Entity20430| |instance-of| |Tangible-Entity|)
          (|_Move20427| |instance-of| |Move|)
          (|_Admit20425| |instance-of| |Admit|)
          (|_Time-Interval20424| |instance-of| |Time-Interval|)
          (|_Be-Shut-Out20423| |instance-of| |Be-Shut-Out|)
          (|_Move20407| |instance-of| |Move|)
          (|_Shut-Out20405| |instance-of| |Shut-Out|)
          (|_Time-Interval20404| |instance-of| |Time-Interval|)
          (|_Portal20331| |instance-of| |Portal|)
          (|_Food20344| |instance-of| |Food|)
          (|_Time-Interval20384| |instance-of| |Time-Interval|)
          (|_Spatial-Entity20379| |instance-of| |Spatial-Entity|)
          (|_Spatial-Entity20378| |instance-of| |Spatial-Entity|)
          (|_Tangible-Entity20383| |instance-of| |Animal|)
          (|_Tangible-Entity20376| |instance-of| |Tangible-Entity|)
          (|_Move20363| |instance-of| |Move|)
          (|_Move20362| |instance-of| |Move|)
          (|_Admit20361| |instance-of| |Admit|)
          (|_Time-Interval20360| |instance-of| |Time-Interval|)
          (|_Spatial-Entity20328| |instance-of| |Spatial-Entity|)
          (|_Spatial-Entity20327| |instance-of| |Spatial-Entity|)
          (|_Spatial-Entity20329| |instance-of| |Spatial-Entity|)
          (|_Tangible-Entity20343| |instance-of| |Tangible-Entity|)
          (|_Be-Contained20359| |instance-of| |Be-Contained|)
          (|_Animal20326| |instance-of| |Animal|)
          (|_Acceleration-Magnitude-Value20334| |instance-of|
           |Acceleration-Magnitude-Value|)
          (|_Acceleration-Vector-Value20333| |instance-of|
           |Acceleration-Vector-Value|)
          (|_Length-Value20340| |instance-of| |Length-Value|)
          (|_Duration-Value20332| |instance-of| |Duration-Value|)
          (|_Speed-Value20336| |instance-of| |Speed-Value|)
          (|_Displacement-Vector-Value20339| |instance-of|
           |Displacement-Vector-Value|)
          (|_Speed-Value20358| |instance-of| |Speed-Value|)
          (|_Velocity-Vector-Value20355| |instance-of|
           |Velocity-Vector-Value|)
          (|_Speed-Value20357| |instance-of| |Speed-Value|)
          (|_Speed-Value20356| |instance-of| |Speed-Value|)
          (|_Speed-Value20354| |instance-of| |Speed-Value|)
          (|_Velocity-Vector-Value20351| |instance-of|
           |Velocity-Vector-Value|)
          (|_Speed-Value20353| |instance-of| |Speed-Value|)
          (|_Speed-Value20352| |instance-of| |Speed-Value|)
          (|_Velocity-Vector-Value20335| |instance-of|
           |Velocity-Vector-Value|)
          (|_Acceleration-Magnitude-Value20350| |instance-of|
           |Acceleration-Magnitude-Value|)
          (|_Length-Value20349| |instance-of| |Length-Value|)
          (|_Speed-Value20348| |instance-of| |Speed-Value|)
          (|_Acceleration-Magnitude-Value20347| |instance-of|
           |Acceleration-Magnitude-Value|)
          (|_Length-Value20346| |instance-of| |Length-Value|)
          (|_Eat20325| |instance-of| |Eat|)
          (|_Speed-Value20345| |instance-of| |Speed-Value|)
          (|_Tangible-Entity20428| |plays| |_Container20435|)
          (|_Tangible-Entity20430| |is-outside|
           |_Tangible-Entity20428|)
          (|_Be-Shut-Out20423| |actions| |_Be-Shut-Out20423|)
          (|_Be-Shut-Out20423| |primitive-actions| |_Be-Shut-Out20423|)
          (|_Be-Shut-Out20423| |time-during| |_Time-Interval20434|)
          (|_Be-Shut-Out20423| |base| |_Tangible-Entity20428|)
          (|_Be-Shut-Out20423| |in-event-of| |_Tangible-Entity20428|)
          (|_Be-Shut-Out20423| |object| |_Tangible-Entity20430|)
          (|_Shut-Out20405| |actions| |_Shut-Out20405|)
          (|_Shut-Out20405| |preparatory-event| |_Move20427|)
          (|_Shut-Out20405| |preparatory-event| |_Admit20425|)
          (|_Shut-Out20405| |primitive-actions| |_Shut-Out20405|)
          (|_Shut-Out20405| |time-during| |_Time-Interval20424|)
          (|_Shut-Out20405| |base| |_Animal20326|)
          (|_Shut-Out20405| |object| |_Tangible-Entity20343|)
          (|_Shut-Out20405| |resulting-state| |_Be-Shut-Out20423|)
          (|_Be-Contained20359| |base| |_Animal20326|)
          (|_Tangible-Entity20343| |object-of| |_Be-Contained20359|)
          (|_Admit20361| |actions| |_Admit20361|)
          (|_Admit20361| |preparatory-event| |_Move20407|)
          (|_Admit20361| |preparatory-event| |_Shut-Out20405|)
          (|_Admit20361| |primitive-actions| |_Admit20361|)
          (|_Admit20361| |time-during| |_Time-Interval20404|)
          (|_Admit20361| |base| |_Animal20326|)
          (|_Admit20361| |object| |_Tangible-Entity20343|)
          (|_Spatial-Entity20328| |is-inside| |_Animal20326|)
          (|_Spatial-Entity20327| |is-outside| |_Animal20326|)
          (|_Spatial-Entity20329| |plays| |_Portal20331|)
          (|_Spatial-Entity20329| |is-region-of| |_Animal20326|)
          (|_Tangible-Entity20343| |destination-of| |_Move20363|)
          (|_Tangible-Entity20343| |plays| |_Food20344|)
          (|_Be-Contained20359| |actions| |_Be-Contained20359|)
          (|_Be-Contained20359| |primitive-actions|
           |_Be-Contained20359|)
          (|_Be-Contained20359| |time-during| |_Time-Interval20384|)
          (|_Be-Contained20359| |destination| |_Spatial-Entity20379|)
          (|_Be-Contained20359| |origin| |_Spatial-Entity20378|)
          (|_Be-Contained20359| |base| |_Tangible-Entity20383|)
          (|_Be-Contained20359| |object| |_Tangible-Entity20376|)
          (|_Eat20325| |actions| |_Eat20325|)
          (|_Eat20325| |preparatory-event| |_Move20363|)
          (|_Eat20325| |preparatory-event| |_Move20362|)
          (|_Eat20325| |preparatory-event| |_Admit20361|)
          (|_Eat20325| |primitive-actions| |_Eat20325|)
          (|_Eat20325| |time-during| |_Time-Interval20360|)
          (|_Eat20325| |destination| |_Spatial-Entity20328|)
          (|_Eat20325| |origin| |_Spatial-Entity20327|)
          (|_Eat20325| |path| |_Spatial-Entity20329|)
          (|_Eat20325| |base| |_Animal20326|)
          (|_Eat20325| |object| |_Tangible-Entity20343|)
          (|_Eat20325| |resulting-state| |_Be-Contained20359|)
          (|_Eat20325| |agent| |_Animal20326|)
          (|_Eat20325| |acceleration-magnitude|
           |_Acceleration-Magnitude-Value20334|)
          (|_Eat20325| |acceleration|
           |_Acceleration-Vector-Value20333|)
          (|_Eat20325| |distance| |_Length-Value20340|)
          (|_Eat20325| |duration| |_Duration-Value20332|)
          (|_Eat20325| |speed| |_Speed-Value20336|)
          (|_Eat20325| |displacement|
           |_Displacement-Vector-Value20339|)
          (|_Eat20325| |final-speed| |_Speed-Value20358|)
          (|_Eat20325| |final-velocity| |_Velocity-Vector-Value20355|)
          (|_Eat20325| |final-x-speed| |_Speed-Value20357|)
          (|_Eat20325| |final-y-speed| |_Speed-Value20356|)
          (|_Eat20325| |initial-speed| |_Speed-Value20354|)
          (|_Eat20325| |initial-velocity|
           |_Velocity-Vector-Value20351|)
          (|_Eat20325| |initial-x-speed| |_Speed-Value20353|)
          (|_Eat20325| |initial-y-speed| |_Speed-Value20352|)
          (|_Eat20325| |velocity| |_Velocity-Vector-Value20335|)
          (|_Eat20325| |x-acceleration-magnitude|
           |_Acceleration-Magnitude-Value20350|)
          (|_Eat20325| |x-distance| |_Length-Value20349|)
          (|_Eat20325| |x-speed| |_Speed-Value20348|)
          (|_Eat20325| |y-acceleration-magnitude|
           |_Acceleration-Magnitude-Value20347|)
          (|_Eat20325| |y-distance| |_Length-Value20346|)
          (|_Eat20325| |y-speed| |_Speed-Value20345|))
         T NIL)))
(|Suborganismal-Entity| (|_Suborganismal-Entity34010|
                         (((|_Suborganismal-Entity34010| |instance-of|
                            |Suborganismal-Entity|)
                           (|_Organism34012| |instance-of| |Organism|)
                           (|_Suborganismal-Entity34010| |is-part-of|
                            |_Organism34012|))
                          T NIL)))
(|Reflexive-Cliche| (|_Reflexive-Cliche34984|
                     (((|_Reflexive-Cliche34984| |instance-of|
                        |Reflexive-Cliche|))
                      T NIL)))
(|Furnishing| (|_Furnishing34162|
               (((|_Time-Interval34167| |instance-of| |Time-Interval|)
                 (|_Furnishing34162| |instance-of| |Furnishing|)
                 (|_Create34164| |instance-of| |Create|)
                 (|_Create34164| |actions| |_Create34164|)
                 (|_Create34164| |primitive-actions| |_Create34164|)
                 (|_Create34164| |time-during| |_Time-Interval34167|)
                 (|_Furnishing34162| |result-of| |_Create34164|))
                T NIL)))
(|Metal| (|_Metal33856|
          (((|_Metal33856| |instance-of| |Metal|)) T NIL)))
(|Taste-Value| (|_Taste-Value37693|
                (((|_Taste-Value37693| |instance-of| |Taste-Value|)) T
                 NIL)))
(|Instrument-Role| (|_Instrument-Role33007|
                    (((|_Instrument-Role33007| |instance-of|
                       |Instrument-Role|)
                      (|_Entity33009| |instance-of| |Entity|)
                      (|_Instrument-Role33007| |played-by|
                       |_Entity33009|))
                     T NIL)))
(|Make-Request| (|_Make-Request31052|
                 (((|_Move31235| |instance-of| |Move|)
                   (|_Time-Interval31234| |instance-of|
                    |Time-Interval|)
                   (|_Signal31197| |instance-of| |Signal|)
                   (|_Move31184| |instance-of| |Move|)
                   (|_Time-Interval31183| |instance-of|
                    |Time-Interval|)
                   (|_Time-Interval31145| |instance-of|
                    |Time-Interval|)
                   (|_Tangible-Entity31144| |instance-of|
                    |Living-Entity|)
                   (|_Entity31140| |instance-of| |Entity|)
                   (|_Move31138| |instance-of| |Move|)
                   (|_Time-Interval31137| |instance-of|
                    |Time-Interval|)
                   (|_Be-Known31136| |instance-of| |Be-Known|)
                   (|_Move31129| |instance-of| |Move|)
                   (|_Time-Interval31128| |instance-of|
                    |Time-Interval|)
                   (|_Tangible-Entity31127| |instance-of|
                    |Tangible-Entity|)
                   (|_Tangible-Entity31120| |instance-of|
                    |Tangible-Entity|)
                   (|_Move31118| |instance-of| |Move|)
                   (|_Time-Interval31117| |instance-of|
                    |Time-Interval|)
                   (|_Tangible-Entity31110| |instance-of|
                    |Tangible-Entity|)
                   (|_Time-Interval31090| |instance-of|
                    |Time-Interval|)
                   (|_Tangible-Entity31089| |instance-of|
                    |Living-Entity|)
                   (|_Entity31085| |instance-of| |Entity|)
                   (|_Categorical31061| |instance-of| |Categorical|)
                   (|_Move31073| |instance-of| |Move|)
                   (|_Sense31072| |instance-of| |Sense|)
                   (|_Transmit31071| |instance-of| |Transmit|)
                   (|_Embody31070| |instance-of| |Embody|)
                   (|_Time-Interval31069| |instance-of|
                    |Time-Interval|)
                   (|_Message31068| |instance-of| |Message|)
                   (|_Message31068| |instance-of| |Spatial-Entity|)
                   (|_Request31063| |instance-of| |Request|)
                   (|_Request31063| |instance-of| |Spatial-Entity|)
                   (|_Tangible-Entity31059| |instance-of|
                    |Tangible-Entity|)
                   (|_Interpret31067| |instance-of| |Interpret|)
                   (|_Convey31066| |instance-of| |Convey|)
                   (|_Express31065| |instance-of| |Express|)
                   (|_Be-Known31064| |instance-of| |Be-Known|)
                   (|_Make-Request31052| |instance-of| |Make-Request|)
                   (|_Manner-Value31060| |instance-of| |Manner-Value|)
                   (|_Tangible-Entity31127| |destination-of|
                    |_Move31235|)
                   (|_Sense31072| |actions-of| |_Sense31072|)
                   (|_Sense31072| |preparatory-event| |_Move31235|)
                   (|_Sense31072| |primitive-actions-of| |_Sense31072|)
                   (|_Sense31072| |time-during| |_Time-Interval31234|)
                   (|_Sense31072| |experiencer|
                    |_Tangible-Entity31059|)
                   (|_Sense31072| |object| |_Tangible-Entity31127|)
                   (|_Sense31072| |result| |_Message31068|)
                   (|_Sense31072| |agent| |_Tangible-Entity31059|)
                   (|_Tangible-Entity31127| |object-of|
                    |_Transmit31071|)
                   (|_Tangible-Entity31127| |plays| |_Signal31197|)
                   (|_Embody31070| |actions-of| |_Embody31070|)
                   (|_Embody31070| |preparatory-event| |_Move31184|)
                   (|_Embody31070| |primitive-actions-of|
                    |_Embody31070|)
                   (|_Embody31070| |time-during| |_Time-Interval31183|)
                   (|_Embody31070| |object| |_Message31068|)
                   (|_Embody31070| |result| |_Tangible-Entity31127|)
                   (|_Embody31070| |next-event| |_Transmit31071|)
                   (|_Embody31070| |agent| |_Tangible-Entity31120|)
                   (|_Be-Known31136| |actions| |_Be-Known31136|)
                   (|_Be-Known31136| |primitive-actions|
                    |_Be-Known31136|)
                   (|_Be-Known31136| |time-during|
                    |_Time-Interval31145|)
                   (|_Be-Known31136| |experiencer|
                    |_Tangible-Entity31144|)
                   (|_Be-Known31136| |object| |_Entity31140|)
                   (|_Interpret31067| |actions-of| |_Interpret31067|)
                   (|_Interpret31067| |preparatory-event| |_Move31138|)
                   (|_Interpret31067| |primitive-actions-of|
                    |_Interpret31067|)
                   (|_Interpret31067| |time-during|
                    |_Time-Interval31137|)
                   (|_Interpret31067| |object| |_Message31068|)
                   (|_Interpret31067| |result| |_Request31063|)
                   (|_Interpret31067| |resulting-state|
                    |_Be-Known31136|)
                   (|_Interpret31067| |agent| |_Tangible-Entity31059|)
                   (|_Convey31066| |actions| |_Sense31072|)
                   (|_Convey31066| |actions| |_Transmit31071|)
                   (|_Convey31066| |actions| |_Embody31070|)
                   (|_Convey31066| |actions-of| |_Convey31066|)
                   (|_Convey31066| |all-subevents| |_Sense31072|)
                   (|_Convey31066| |all-subevents| |_Transmit31071|)
                   (|_Convey31066| |all-subevents| |_Embody31070|)
                   (|_Convey31066| |preparatory-event| |_Move31129|)
                   (|_Convey31066| |primitive-actions| |_Sense31072|)
                   (|_Convey31066| |primitive-actions|
                    |_Transmit31071|)
                   (|_Convey31066| |primitive-actions| |_Embody31070|)
                   (|_Convey31066| |time-during| |_Time-Interval31128|)
                   (|_Convey31066| |base| |_Tangible-Entity31127|)
                   (|_Convey31066| |object| |_Message31068|)
                   (|_Convey31066| |recipient| |_Tangible-Entity31059|)
                   (|_Convey31066| |first-subevent| |_Embody31070|)
                   (|_Convey31066| |next-event| |_Interpret31067|)
                   (|_Convey31066| |subevent| |_Sense31072|)
                   (|_Convey31066| |subevent| |_Transmit31071|)
                   (|_Convey31066| |subevent| |_Embody31070|)
                   (|_Convey31066| |agent| |_Tangible-Entity31120|)
                   (|_Express31065| |actions-of| |_Express31065|)
                   (|_Express31065| |preparatory-event| |_Move31118|)
                   (|_Express31065| |primitive-actions-of|
                    |_Express31065|)
                   (|_Express31065| |time-during|
                    |_Time-Interval31117|)
                   (|_Express31065| |object| |_Request31063|)
                   (|_Express31065| |result| |_Message31068|)
                   (|_Express31065| |next-event| |_Convey31066|)
                   (|_Express31065| |agent| |_Tangible-Entity31110|)
                   (|_Be-Known31064| |actions| |_Be-Known31064|)
                   (|_Be-Known31064| |primitive-actions|
                    |_Be-Known31064|)
                   (|_Be-Known31064| |time-during|
                    |_Time-Interval31090|)
                   (|_Be-Known31064| |experiencer|
                    |_Tangible-Entity31089|)
                   (|_Be-Known31064| |object| |_Entity31085|)
                   (|_Manner-Value31060| |categorical-value|
                    |_Categorical31061|)
                   (|_Make-Request31052| |actions|
                    |_Make-Request31052|)
                   (|_Make-Request31052| |actions| |_Interpret31067|)
                   (|_Make-Request31052| |actions| |_Convey31066|)
                   (|_Make-Request31052| |actions| |_Sense31072|)
                   (|_Make-Request31052| |actions| |_Transmit31071|)
                   (|_Make-Request31052| |actions| |_Embody31070|)
                   (|_Make-Request31052| |actions| |_Express31065|)
                   (|_Make-Request31052| |all-subevents| |_Sense31072|)
                   (|_Make-Request31052| |all-subevents|
                    |_Transmit31071|)
                   (|_Make-Request31052| |all-subevents|
                    |_Embody31070|)
                   (|_Make-Request31052| |all-subevents|
                    |_Interpret31067|)
                   (|_Make-Request31052| |all-subevents|
                    |_Convey31066|)
                   (|_Make-Request31052| |all-subevents|
                    |_Express31065|)
                   (|_Make-Request31052| |preparatory-event|
                    |_Move31073|)
                   (|_Make-Request31052| |primitive-actions|
                    |_Interpret31067|)
                   (|_Make-Request31052| |primitive-actions|
                    |_Sense31072|)
                   (|_Make-Request31052| |primitive-actions|
                    |_Transmit31071|)
                   (|_Make-Request31052| |primitive-actions|
                    |_Embody31070|)
                   (|_Make-Request31052| |primitive-actions|
                    |_Express31065|)
                   (|_Make-Request31052| |time-during|
                    |_Time-Interval31069|)
                   (|_Make-Request31052| |base| |_Message31068|)
                   (|_Make-Request31052| |object| |_Request31063|)
                   (|_Make-Request31052| |recipient|
                    |_Tangible-Entity31059|)
                   (|_Make-Request31052| |first-subevent|
                    |_Express31065|)
                   (|_Make-Request31052| |subevent| |_Interpret31067|)
                   (|_Make-Request31052| |subevent| |_Convey31066|)
                   (|_Make-Request31052| |subevent| |_Express31065|)
                   (|_Make-Request31052| |resulting-state|
                    |_Be-Known31064|)
                   (|_Make-Request31052| |manner|
                    |_Manner-Value31060|))
                  T NIL)))
(|Portal| (|_Portal33054|
           (((|_Move-Through33056| |instance-of| |Move-Through|)
             (|_Portal33054| |instance-of| |Portal|)
             (|_Spatial-Entity33055| |instance-of| |Spatial-Entity|)
             (|_Portal33054| |in-event| |_Move-Through33056|)
             (|_Portal33054| |played-by| |_Spatial-Entity33055|))
            T NIL)))
(|Circumference-Scale| (|_Circumference-Scale37627|
                        (((|_Circumference-Scale37627| |instance-of|
                           |Circumference-Scale|)
                          (|_Number37629| |instance-of| |Number|)
                          (|_Circumference-Scale37627|
                           |number-of-elements| |_Number37629|))
                         T NIL)))
(|Tangible-Entity| (|_Tangible-Entity33604|
                    (((|_Tangible-Entity33604| |instance-of|
                       |Tangible-Entity|))
                     T NIL)))
(|Physical-Object| (|_Physical-Object33983|
                    (((|_Physical-Object33983| |instance-of|
                       |Physical-Object|))
                     T NIL)))
(|Temperature-Scale| (|_Temperature-Scale37522|
                      (((|_Temperature-Scale37522| |instance-of|
                         |Temperature-Scale|)
                        (|_Number37524| |instance-of| |Number|)
                        (|_Temperature-Scale37522| |number-of-elements|
                         |_Number37524|))
                       T NIL)))
(|Velocity-Scale| (|_Velocity-Scale37512|
                   (((|_Velocity-Scale37512| |instance-of|
                      |Velocity-Scale|)
                     (|_Number37514| |instance-of| |Number|)
                     (|_Velocity-Scale37512| |number-of-elements|
                      |_Number37514|))
                    T NIL)))
(|Slot-Query-Viewpoint| (|_Slot-Query-Viewpoint34928|
                         (((|_Slot-Query-Viewpoint34928| |instance-of|
                            |Slot-Query-Viewpoint|))
                          T NIL)))
(|Number| (|_Number33598|
           (((|_Number33598| |instance-of| |Number|)) T NIL)))
(|Meronymic-Relation| (|_Meronymic-Relation37947|
                       (((|_Meronymic-Relation37947| |instance-of|
                          |Meronymic-Relation|))
                        T NIL)))
(|Equation-Set| (|_Equation-Set34974|
                 (((|_Equation-Set34974| |instance-of| |Equation-Set|))
                  T NIL)))
(|Rubber| (|_Rubber33732|
           (((|_Categorical33737| |instance-of| |Categorical|)
             (|_Rubber33732| |instance-of| |Rubber|)
             (|_State-Value33736| |instance-of| |State-Value|)
             (|_State-Value33736| |categorical-value|
              |_Categorical33737|)
             (|_Rubber33732| |physical-state| |_State-Value33736|))
            T NIL)))
(|Salient-Node| (|_Salient-Node34912|
                 (((|_Salient-Node34912| |instance-of| |Salient-Node|))
                  T NIL)))
(|Operations| (|_Operations516|
               (((|_Operations516| |instance-of| |Operations|)
                 (|_Time-Interval522| |instance-of| |Time-Interval|)
                 (|_Operations516| |actions| |_Operations516|)
                 (|_Operations516| |primitive-actions|
                  |_Operations516|)
                 (|_Operations516| |time-during| |_Time-Interval522|))
                T NIL)))
(|Recording| (|_Recording34754|
              (((|_Language34757| |instance-of| |Language|)
                (|_Record34756| |instance-of| |Record|)
                (|_Recording34754| |instance-of| |Recording|)
                (|_Thing34755| |instance-of| |Thing|)
                (|_Recording34754| |information-language|
                 |_Language34757|)
                (|_Recording34754| |result-of| |_Record34756|)
                (|_Recording34754| |information-content|
                 |_Thing34755|))
               T NIL)))
(|Importance-Scale| (|_Importance-Scale37582|
                     (((|_Importance-Scale37582| |instance-of|
                        |Importance-Scale|)
                       (|_Number37584| |instance-of| |Number|)
                       (|_Importance-Scale37582| |number-of-elements|
                        |_Number37584|))
                      T NIL)))
(|Depth-Scale| (|_Depth-Scale37617|
                (((|_Depth-Scale37617| |instance-of| |Depth-Scale|)
                  (|_Number37619| |instance-of| |Number|)
                  (|_Depth-Scale37617| |number-of-elements|
                   |_Number37619|))
                 T NIL)))
(|PH-Value| (|_PH-Value37682|
             (((|_PH-Value37682| |instance-of| |PH-Value|)) T NIL)))
(|Place-Order| (|_Place-Order10020|
                (((|_Move10028| |instance-of| |Move|)
                  (|_Time-Interval10027| |instance-of| |Time-Interval|)
                  (|_Entity10026| |instance-of| |Entity|)
                  (|_Place-Order10020| |instance-of| |Place-Order|)
                  (|_Entity10024| |instance-of| |Spatial-Entity|)
                  (|_Entity10024| |is-possessed-by| |_Entity10026|)
                  (|_Place-Order10020| |actions| |_Place-Order10020|)
                  (|_Place-Order10020| |preparatory-event|
                   |_Move10028|)
                  (|_Place-Order10020| |primitive-actions|
                   |_Place-Order10020|)
                  (|_Place-Order10020| |time-during|
                   |_Time-Interval10027|)
                  (|_Place-Order10020| |base| |_Entity10026|)
                  (|_Place-Order10020| |object| |_Entity10024|))
                 T NIL)))
(|Dry| (|_Dry25383|
        (((|_Time-Interval25394| |instance-of| |Time-Interval|)
          (|_Tangible-Entity25393| |instance-of| |Tangible-Entity|)
          (|_Wetness-Value25389| |instance-of| |Wetness-Value|)
          (|_Dry25383| |instance-of| |Dry|)
          (|_Wetness-Value25390| |instance-of| |Wetness-Value|)
          (|_Wetness-Value25390| |less-than| |_Wetness-Value25389|)
          (|_Dry25383| |actions| |_Dry25383|)
          (|_Dry25383| |primitive-actions| |_Dry25383|)
          (|_Dry25383| |time-during| |_Time-Interval25394|)
          (|_Dry25383| |base| |_Tangible-Entity25393|)
          (|_Dry25383| |from-value| |_Wetness-Value25389|)
          (|_Dry25383| |to-value| |_Wetness-Value25390|))
         T NIL)))
(|Family| (|_Family34872|
           (((|_Family34872| |instance-of| |Family|)
             (|_Number34874| |instance-of| |Number|)
             (|_Family34872| |number-of-elements| |_Number34874|))
            T NIL)))
(|Unobstruct| (|_Unobstruct24163|
               (((|_Move24171| |instance-of| |Move|)
                 (|_Make-Inaccessible24170| |instance-of|
                  |Make-Inaccessible|)
                 (|_Time-Interval24169| |instance-of| |Time-Interval|)
                 (|_Unobstruct24163| |instance-of| |Unobstruct|)
                 (|_Entity24167| |instance-of| |Spatial-Entity|)
                 (|_Unobstruct24163| |actions| |_Unobstruct24163|)
                 (|_Unobstruct24163| |preparatory-event| |_Move24171|)
                 (|_Unobstruct24163| |preparatory-event|
                  |_Make-Inaccessible24170|)
                 (|_Unobstruct24163| |primitive-actions|
                  |_Unobstruct24163|)
                 (|_Unobstruct24163| |time-during|
                  |_Time-Interval24169|)
                 (|_Unobstruct24163| |object| |_Entity24167|))
                T NIL)))
(|UoM-Unitless| (|_UoM-Unitless37430|
                 (((|_UoM-Unitless37430| |instance-of| |UoM-Unitless|))
                  T NIL)))
(|Animacy-Value| (|_Animacy-Value37758|
                  (((|_Animacy-Value37758| |instance-of|
                     |Animacy-Value|))
                   T NIL)))
(|Shelf| (|_Shelf34178|
          (((|_Time-Interval34183| |instance-of| |Time-Interval|)
            (|_Shelf34178| |instance-of| |Shelf|)
            (|_Create34180| |instance-of| |Create|)
            (|_Create34180| |actions| |_Create34180|)
            (|_Create34180| |primitive-actions| |_Create34180|)
            (|_Create34180| |time-during| |_Time-Interval34183|)
            (|_Shelf34178| |result-of| |_Create34180|))
           T NIL)))
(|Move-Together| (|_Move-Together23433|
                  (((|_Move23450| |instance-of| |Move|)
                    (|_Time-Interval23447| |instance-of|
                     |Time-Interval|)
                    (|_Spatial-Entity23439| |instance-of|
                     |Spatial-Entity|)
                    (|_Tangible-Entity23442| |instance-of|
                     |Tangible-Entity|)
                    (|_Tangible-Entity23441| |instance-of|
                     |Tangible-Entity|)
                    (|_Move23444| |instance-of| |Move|)
                    (|_Move-Together23433| |instance-of|
                     |Move-Together|)
                    (|_Move23443| |instance-of| |Move|)
                    (|_Move-Together23433| |actions|
                     |_Move-Together23433|)
                    (|_Move-Together23433| |actions| |_Move23444|)
                    (|_Move-Together23433| |actions| |_Move23443|)
                    (|_Move-Together23433| |all-subevents|
                     |_Move23444|)
                    (|_Move-Together23433| |all-subevents|
                     |_Move23443|)
                    (|_Move-Together23433| |preparatory-event|
                     |_Move23450|)
                    (|_Move-Together23433| |primitive-actions|
                     |_Move23444|)
                    (|_Move-Together23433| |primitive-actions|
                     |_Move23443|)
                    (|_Move-Together23433| |time-during|
                     |_Time-Interval23447|)
                    (|_Move-Together23433| |destination|
                     |_Spatial-Entity23439|)
                    (|_Move-Together23433| |object|
                     |_Tangible-Entity23442|)
                    (|_Move-Together23433| |object|
                     |_Tangible-Entity23441|)
                    (|_Move-Together23433| |first-subevent|
                     |_Move23444|)
                    (|_Move-Together23433| |first-subevent|
                     |_Move23443|)
                    (|_Move-Together23433| |subevent| |_Move23444|)
                    (|_Move-Together23433| |subevent| |_Move23443|))
                   T NIL)))
(|Hallway| (|_Hallway34279|
            (((|_Time-Interval34320| |instance-of| |Time-Interval|)
              (|_Time-Interval34317| |instance-of| |Time-Interval|)
              (|_Time-Interval34314| |instance-of| |Time-Interval|)
              (|_Time-Interval34309| |instance-of| |Time-Interval|)
              (|_Time-Interval34306| |instance-of| |Time-Interval|)
              (|_Time-Interval34303| |instance-of| |Time-Interval|)
              (|_Scalar34290| |instance-of| |Scalar|)
              (|_Be-Stable34294| |instance-of| |Be-Stable|)
              (|_Be-Supported34293| |instance-of| |Be-Supported|)
              (|_Cover34292| |instance-of| |Cover|)
              (|_Create34291| |instance-of| |Create|)
              (|_Angle-Value34289| |instance-of| |Angle-Value|)
              (|_Be-Stable34288| |instance-of| |Be-Stable|)
              (|_Be-Supported34287| |instance-of| |Be-Supported|)
              (|_Container34286| |instance-of| |Container|)
              (|_Create34285| |instance-of| |Create|)
              (|_Ceiling34284| |instance-of| |Ceiling|)
              (|_Wall34283| |instance-of| |Wall|)
              (|_Floor34282| |instance-of| |Floor|)
              (|_Hallway34279| |instance-of| |Hallway|)
              (|_Building34281| |instance-of| |Building|)
              (|_Be-Stable34288| |actions| |_Be-Stable34288|)
              (|_Be-Stable34288| |primitive-actions| |_Be-Stable34288|)
              (|_Be-Stable34288| |time-during| |_Time-Interval34320|)
              (|_Be-Supported34287| |actions| |_Be-Supported34287|)
              (|_Be-Supported34287| |primitive-actions|
               |_Be-Supported34287|)
              (|_Be-Supported34287| |time-during|
               |_Time-Interval34317|)
              (|_Create34285| |actions| |_Create34285|)
              (|_Create34285| |primitive-actions| |_Create34285|)
              (|_Create34285| |time-during| |_Time-Interval34314|)
              (|_Be-Stable34294| |actions| |_Be-Stable34294|)
              (|_Be-Stable34294| |primitive-actions| |_Be-Stable34294|)
              (|_Be-Stable34294| |time-during| |_Time-Interval34309|)
              (|_Be-Supported34293| |actions| |_Be-Supported34293|)
              (|_Be-Supported34293| |primitive-actions|
               |_Be-Supported34293|)
              (|_Be-Supported34293| |time-during|
               |_Time-Interval34306|)
              (|_Create34291| |actions| |_Create34291|)
              (|_Create34291| |primitive-actions| |_Create34291|)
              (|_Create34291| |time-during| |_Time-Interval34303|)
              (|_Angle-Value34289| |scalar-value| |_Scalar34290|)
              (|_Ceiling34284| |is-above| |_Hallway34279|)
              (|_Ceiling34284| |object-of| |_Be-Stable34294|)
              (|_Ceiling34284| |object-of| |_Be-Supported34293|)
              (|_Ceiling34284| |plays| |_Cover34292|)
              (|_Ceiling34284| |result-of| |_Create34291|)
              (|_Ceiling34284| |orientation| |_Angle-Value34289|)
              (|_Hallway34279| |object-of| |_Be-Stable34288|)
              (|_Hallway34279| |object-of| |_Be-Supported34287|)
              (|_Hallway34279| |plays| |_Container34286|)
              (|_Hallway34279| |result-of| |_Create34285|)
              (|_Hallway34279| |has-part| |_Ceiling34284|)
              (|_Hallway34279| |has-part| |_Wall34283|)
              (|_Hallway34279| |has-part| |_Floor34282|)
              (|_Hallway34279| |is-part-of| |_Building34281|))
             T NIL)))
(|Unit-Conversion| (|_Unit-Conversion34946|
                    (((|_Property-Value34949| |instance-of|
                       |Property-Value|)
                      (|_Unit-Conversion34946| |instance-of|
                       |Unit-Conversion|)
                      (|_Unit-of-Measurement34948| |instance-of|
                       |Unit-of-Measurement|)
                      (|_Unit-Conversion34946| |input|
                       |_Property-Value34949|)
                      (|_Unit-Conversion34946| |target-unit|
                       |_Unit-of-Measurement34948|))
                     T NIL)))
(|UoM-Rotational-Rate| (|_UoM-Rotational-Rate37438|
                        (((|_UoM-Rotational-Rate37438| |instance-of|
                           |UoM-Rotational-Rate|))
                         T NIL)))
(|PH-Constant| (|_PH-Constant37797|
                (((|_PH-Constant37797| |instance-of| |PH-Constant|)) T
                 NIL)))
(|Displacement-Vector-Value| (|_Displacement-Vector-Value37772|
                              (((|_Angle-Value37774|
                                 |instance-of|
                                 |Angle-Value|)
                                (|_Length-Value37773|
                                 |instance-of|
                                 |Length-Value|)
                                (|_Length-Value37776|
                                 |instance-of|
                                 |Length-Value|)
                                (|_Displacement-Vector-Value37772|
                                 |instance-of|
                                 |Displacement-Vector-Value|)
                                (|_Length-Value37775|
                                 |instance-of|
                                 |Length-Value|)
                                (|_Displacement-Vector-Value37772|
                                 |x-component-slot|
                                 |x-distance|)
                                (|_Displacement-Vector-Value37772|
                                 |y-component-slot|
                                 |y-distance|)
                                (|_Displacement-Vector-Value37772|
                                 |direction|
                                 |_Angle-Value37774|)
                                (|_Displacement-Vector-Value37772|
                                 |distance|
                                 |_Length-Value37773|)
                                (|_Displacement-Vector-Value37772|
                                 |x-distance|
                                 |_Length-Value37776|)
                                (|_Displacement-Vector-Value37772|
                                 |y-distance|
                                 |_Length-Value37775|))
                               T NIL)))
(|Information| (|_Information34795|
                (((|_Information34795| |instance-of| |Information|)
                  (|_Thing34796| |instance-of| |Thing|)
                  (|_Information34795| |information-content|
                   |_Thing34796|))
                 T NIL)))
(|Rate-Scale| (|_Rate-Scale37537|
               (((|_Rate-Scale37537| |instance-of| |Rate-Scale|)
                 (|_Number37539| |instance-of| |Number|)
                 (|_Rate-Scale37537| |number-of-elements|
                  |_Number37539|))
                T NIL)))
(|Piece-of-Tissue| (|_Piece-of-Tissue34054|
                    (((|_Piece-of-Tissue34054| |instance-of|
                       |Piece-of-Tissue|)
                      (|_Tissue34057| |instance-of| |Tissue|)
                      (|_Piece-of-Tissue34054| |material|
                       |_Tissue34057|))
                     T NIL)))
(|Rotate| (|_Rotate10729|
           (((|_Move10769| |instance-of| |Move|)
             (|_Time-Interval10768| |instance-of| |Time-Interval|)
             (|_Tangible-Entity10767| |instance-of| |Tangible-Entity|)
             (|_Acceleration-Magnitude-Value10744| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Acceleration-Vector-Value10743| |instance-of|
              |Acceleration-Vector-Value|)
             (|_Length-Value10750| |instance-of| |Length-Value|)
             (|_Duration-Value10742| |instance-of| |Duration-Value|)
             (|_Speed-Value10746| |instance-of| |Speed-Value|)
             (|_Displacement-Vector-Value10749| |instance-of|
              |Displacement-Vector-Value|)
             (|_Speed-Value10766| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value10763| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value10765| |instance-of| |Speed-Value|)
             (|_Speed-Value10764| |instance-of| |Speed-Value|)
             (|_Speed-Value10762| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value10759| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value10761| |instance-of| |Speed-Value|)
             (|_Speed-Value10760| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value10745| |instance-of|
              |Velocity-Vector-Value|)
             (|_Acceleration-Magnitude-Value10758| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value10757| |instance-of| |Length-Value|)
             (|_Speed-Value10756| |instance-of| |Speed-Value|)
             (|_Acceleration-Magnitude-Value10755| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value10754| |instance-of| |Length-Value|)
             (|_Rotate10729| |instance-of| |Rotate|)
             (|_Speed-Value10753| |instance-of| |Speed-Value|)
             (|_Rotate10729| |actions| |_Rotate10729|)
             (|_Rotate10729| |preparatory-event| |_Move10769|)
             (|_Rotate10729| |primitive-actions| |_Rotate10729|)
             (|_Rotate10729| |time-during| |_Time-Interval10768|)
             (|_Rotate10729| |object| |_Tangible-Entity10767|)
             (|_Rotate10729| |acceleration-magnitude|
              |_Acceleration-Magnitude-Value10744|)
             (|_Rotate10729| |acceleration|
              |_Acceleration-Vector-Value10743|)
             (|_Rotate10729| |distance| |_Length-Value10750|)
             (|_Rotate10729| |duration| |_Duration-Value10742|)
             (|_Rotate10729| |speed| |_Speed-Value10746|)
             (|_Rotate10729| |displacement|
              |_Displacement-Vector-Value10749|)
             (|_Rotate10729| |final-speed| |_Speed-Value10766|)
             (|_Rotate10729| |final-velocity|
              |_Velocity-Vector-Value10763|)
             (|_Rotate10729| |final-x-speed| |_Speed-Value10765|)
             (|_Rotate10729| |final-y-speed| |_Speed-Value10764|)
             (|_Rotate10729| |initial-speed| |_Speed-Value10762|)
             (|_Rotate10729| |initial-velocity|
              |_Velocity-Vector-Value10759|)
             (|_Rotate10729| |initial-x-speed| |_Speed-Value10761|)
             (|_Rotate10729| |initial-y-speed| |_Speed-Value10760|)
             (|_Rotate10729| |velocity| |_Velocity-Vector-Value10745|)
             (|_Rotate10729| |x-acceleration-magnitude|
              |_Acceleration-Magnitude-Value10758|)
             (|_Rotate10729| |x-distance| |_Length-Value10757|)
             (|_Rotate10729| |x-speed| |_Speed-Value10756|)
             (|_Rotate10729| |y-acceleration-magnitude|
              |_Acceleration-Magnitude-Value10755|)
             (|_Rotate10729| |y-distance| |_Length-Value10754|)
             (|_Rotate10729| |y-speed| |_Speed-Value10753|))
            T NIL)))
(|String| (|_String37983|
           (((|_String37983| |instance-of| |String|)) T NIL)))
(|Relinquish| (|_Relinquish1290|
               (((|_Move1307| |instance-of| |Move|)
                 (|_Obtain1306| |instance-of| |Obtain|)
                 (|_Time-Interval1305| |instance-of| |Time-Interval|)
                 (|_Move1300| |instance-of| |Move|)
                 (|_Obtain1299| |instance-of| |Obtain|)
                 (|_Time-Interval1298| |instance-of| |Time-Interval|)
                 (|_Entity1297| |instance-of| |Spatial-Entity|)
                 (|_Relinquish1290| |instance-of| |Relinquish|)
                 (|_Tangible-Entity1295| |instance-of|
                  |Tangible-Entity|)
                 (|_Obtain1299| |actions| |_Obtain1299|)
                 (|_Obtain1299| |preparatory-event| |_Move1307|)
                 (|_Obtain1299| |preparatory-event| |_Obtain1306|)
                 (|_Obtain1299| |primitive-actions| |_Obtain1299|)
                 (|_Obtain1299| |time-during| |_Time-Interval1305|)
                 (|_Obtain1299| |object| |_Entity1297|)
                 (|_Obtain1299| |recipient| |_Tangible-Entity1295|)
                 (|_Obtain1299| |agent| |_Tangible-Entity1295|)
                 (|_Relinquish1290| |actions| |_Relinquish1290|)
                 (|_Relinquish1290| |preparatory-event| |_Move1300|)
                 (|_Relinquish1290| |preparatory-event| |_Obtain1299|)
                 (|_Relinquish1290| |primitive-actions|
                  |_Relinquish1290|)
                 (|_Relinquish1290| |time-during| |_Time-Interval1298|)
                 (|_Relinquish1290| |object| |_Entity1297|)
                 (|_Relinquish1290| |agent| |_Tangible-Entity1295|)
                 (|_Relinquish1290| |donor| |_Tangible-Entity1295|))
                T NIL)))
(|Deceive| (|_Deceive25122|
            (((|_Time-Interval25152| |instance-of| |Time-Interval|)
              (|_Tangible-Entity25151| |instance-of| |Living-Entity|)
              (|_Entity25147| |instance-of| |Entity|)
              (|_Move25145| |instance-of| |Move|)
              (|_Time-Interval25144| |instance-of| |Time-Interval|)
              (|_Be-Known25143| |instance-of| |Be-Known|)
              (|_Move25136| |instance-of| |Move|)
              (|_Time-Interval25135| |instance-of| |Time-Interval|)
              (|_Information25132| |instance-of| |Information|)
              (|_Information25132| |instance-of| |Spatial-Entity|)
              (|_Tangible-Entity25130| |instance-of| |Tangible-Entity|)
              (|_Message25133| |instance-of| |Message|)
              (|_Message25133| |instance-of| |Spatial-Entity|)
              (|_Interpret25134| |instance-of| |Interpret|)
              (|_Deceive25122| |instance-of| |Deceive|)
              (|_Tangible-Entity25129| |instance-of| |Tangible-Entity|)
              (|_Be-Known25143| |actions| |_Be-Known25143|)
              (|_Be-Known25143| |primitive-actions| |_Be-Known25143|)
              (|_Be-Known25143| |time-during| |_Time-Interval25152|)
              (|_Be-Known25143| |experiencer| |_Tangible-Entity25151|)
              (|_Be-Known25143| |object| |_Entity25147|)
              (|_Interpret25134| |actions| |_Interpret25134|)
              (|_Interpret25134| |preparatory-event| |_Move25145|)
              (|_Interpret25134| |primitive-actions| |_Interpret25134|)
              (|_Interpret25134| |time-during| |_Time-Interval25144|)
              (|_Interpret25134| |object| |_Message25133|)
              (|_Interpret25134| |result| |_Information25132|)
              (|_Interpret25134| |resulting-state| |_Be-Known25143|)
              (|_Interpret25134| |agent| |_Tangible-Entity25130|)
              (|_Deceive25122| |actions| |_Deceive25122|)
              (|_Deceive25122| |preparatory-event| |_Move25136|)
              (|_Deceive25122| |primitive-actions| |_Deceive25122|)
              (|_Deceive25122| |time-during| |_Time-Interval25135|)
              (|_Deceive25122| |object| |_Information25132|)
              (|_Deceive25122| |recipient| |_Tangible-Entity25130|)
              (|_Deceive25122| |result| |_Message25133|)
              (|_Deceive25122| |causes| |_Interpret25134|)
              (|_Deceive25122| |objective| |_Interpret25134|)
              (|_Deceive25122| |agent| |_Tangible-Entity25129|))
             T NIL)))
(|Come-Together| (|_Come-Together32668|
                  (((|_Move32685| |instance-of| |Move|)
                    (|_Time-Interval32682| |instance-of|
                     |Time-Interval|)
                    (|_Spatial-Entity32674| |instance-of|
                     |Spatial-Entity|)
                    (|_Tangible-Entity32677| |instance-of|
                     |Tangible-Entity|)
                    (|_Tangible-Entity32676| |instance-of|
                     |Tangible-Entity|)
                    (|_Go-To32679| |instance-of| |Go-To|)
                    (|_Come-Together32668| |instance-of|
                     |Come-Together|)
                    (|_Go-To32678| |instance-of| |Go-To|)
                    (|_Come-Together32668| |actions|
                     |_Come-Together32668|)
                    (|_Come-Together32668| |actions| |_Go-To32679|)
                    (|_Come-Together32668| |actions| |_Go-To32678|)
                    (|_Come-Together32668| |all-subevents|
                     |_Go-To32679|)
                    (|_Come-Together32668| |all-subevents|
                     |_Go-To32678|)
                    (|_Come-Together32668| |preparatory-event|
                     |_Move32685|)
                    (|_Come-Together32668| |primitive-actions|
                     |_Go-To32679|)
                    (|_Come-Together32668| |primitive-actions|
                     |_Go-To32678|)
                    (|_Come-Together32668| |time-during|
                     |_Time-Interval32682|)
                    (|_Come-Together32668| |destination|
                     |_Spatial-Entity32674|)
                    (|_Come-Together32668| |object|
                     |_Tangible-Entity32677|)
                    (|_Come-Together32668| |object|
                     |_Tangible-Entity32676|)
                    (|_Come-Together32668| |first-subevent|
                     |_Go-To32679|)
                    (|_Come-Together32668| |first-subevent|
                     |_Go-To32678|)
                    (|_Come-Together32668| |subevent| |_Go-To32679|)
                    (|_Come-Together32668| |subevent| |_Go-To32678|))
                   T NIL)))
(|Compute-Maximum| (|_Compute-Maximum34962|
                    (((|_Compute-Maximum34962| |instance-of|
                       |Compute-Maximum|))
                     T NIL)))
(|Receive| (|_Receive1014|
            (((|_Move1024| |instance-of| |Move|)
              (|_Obtain1023| |instance-of| |Obtain|)
              (|_Time-Interval1022| |instance-of| |Time-Interval|)
              (|_Entity1021| |instance-of| |Spatial-Entity|)
              (|_Receive1014| |instance-of| |Receive|)
              (|_Entity1019| |instance-of| |Entity|)
              (|_Receive1014| |actions| |_Receive1014|)
              (|_Receive1014| |preparatory-event| |_Move1024|)
              (|_Receive1014| |preparatory-event| |_Obtain1023|)
              (|_Receive1014| |primitive-actions| |_Receive1014|)
              (|_Receive1014| |time-during| |_Time-Interval1022|)
              (|_Receive1014| |object| |_Entity1021|)
              (|_Receive1014| |recipient| |_Entity1019|))
             T NIL)))
(|Improvement| (|_Improvement821|
                (((|_Improvement821| |instance-of| |Improvement|)
                  (|_Time-Interval827| |instance-of| |Time-Interval|)
                  (|_Improvement821| |actions| |_Improvement821|)
                  (|_Improvement821| |primitive-actions|
                   |_Improvement821|)
                  (|_Improvement821| |time-during|
                   |_Time-Interval827|))
                 T NIL)))
(|Authorize| (|_Authorize32775|
              (((|_Move32784| |instance-of| |Move|)
                (|_Time-Interval32783| |instance-of| |Time-Interval|)
                (|_Authorized32782| |instance-of| |Authorized|)
                (|_Entity32781| |instance-of| |Entity|)
                (|_Authorize32775| |instance-of| |Authorize|)
                (|_Entity32779| |instance-of| |Entity|)
                (|_Authorize32775| |actions| |_Authorize32775|)
                (|_Authorize32775| |preparatory-event| |_Move32784|)
                (|_Authorize32775| |primitive-actions|
                 |_Authorize32775|)
                (|_Authorize32775| |time-during| |_Time-Interval32783|)
                (|_Authorize32775| |enables| |_Authorized32782|)
                (|_Authorize32775| |is-goal-of| |_Entity32781|)
                (|_Authorize32775| |agent| |_Entity32779|))
               T NIL)))
(|Voltage-Value| (|_Voltage-Value37677|
                  (((|_Voltage-Value37677| |instance-of|
                     |Voltage-Value|))
                   T NIL)))
(|Multicellular-Organism| (|_Multicellular-Organism34015|
                           (((|_Multicellular-Organism34015|
                              |instance-of| |Multicellular-Organism|))
                            T NIL)))
(|Deactivate| (|_Deactivate23878|
               (((|_Time-Interval23891| |instance-of| |Time-Interval|)
                 (|_Entity23887| |instance-of| |Entity|)
                 (|_Move23885| |instance-of| |Move|)
                 (|_Activate23883| |instance-of| |Activate|)
                 (|_Time-Interval23882| |instance-of| |Time-Interval|)
                 (|_Entity23879| |instance-of| |Spatial-Entity|)
                 (|_Deactivate23878| |instance-of| |Deactivate|)
                 (|_Be-Inaccessible23881| |instance-of|
                  |Be-Inaccessible|)
                 (|_Be-Inaccessible23881| |actions|
                  |_Be-Inaccessible23881|)
                 (|_Be-Inaccessible23881| |primitive-actions|
                  |_Be-Inaccessible23881|)
                 (|_Be-Inaccessible23881| |time-during|
                  |_Time-Interval23891|)
                 (|_Be-Inaccessible23881| |object| |_Entity23887|)
                 (|_Deactivate23878| |actions| |_Deactivate23878|)
                 (|_Deactivate23878| |preparatory-event| |_Move23885|)
                 (|_Deactivate23878| |preparatory-event|
                  |_Activate23883|)
                 (|_Deactivate23878| |primitive-actions|
                  |_Deactivate23878|)
                 (|_Deactivate23878| |time-during|
                  |_Time-Interval23882|)
                 (|_Deactivate23878| |object| |_Entity23879|)
                 (|_Deactivate23878| |resulting-state|
                  |_Be-Inaccessible23881|))
                T NIL)))
(|Renewable-Resource| (|_Renewable-Resource32962|
                       (((|_Renewable-Resource32962| |instance-of|
                          |Renewable-Resource|)
                         (|_Replenish32964| |instance-of| |Replenish|)
                         (|_Renewable-Resource32962| |base-of|
                          |_Replenish32964|))
                        T NIL)))
(|Manner-Value| (|_Manner-Value37724|
                 (((|_Manner-Value37724| |instance-of| |Manner-Value|))
                  T NIL)))
(|Support| (|_Support23658|
            (((|_Move23679| |instance-of| |Move|)
              (|_Make-Inaccessible23678| |instance-of|
               |Make-Inaccessible|)
              (|_Time-Interval23677| |instance-of| |Time-Interval|)
              (|_Time-Interval23670| |instance-of| |Time-Interval|)
              (|_Entity23666| |instance-of| |Tangible-Entity|)
              (|_Move23664| |instance-of| |Move|)
              (|_Make-Accessible23663| |instance-of| |Make-Accessible|)
              (|_Time-Interval23662| |instance-of| |Time-Interval|)
              (|_Tangible-Entity23659| |instance-of| |Tangible-Entity|)
              (|_Support23658| |instance-of| |Support|)
              (|_Be-Supported23661| |instance-of| |Be-Supported|)
              (|_Make-Accessible23663| |actions|
               |_Make-Accessible23663|)
              (|_Make-Accessible23663| |preparatory-event|
               |_Move23679|)
              (|_Make-Accessible23663| |preparatory-event|
               |_Make-Inaccessible23678|)
              (|_Make-Accessible23663| |primitive-actions|
               |_Make-Accessible23663|)
              (|_Make-Accessible23663| |time-during|
               |_Time-Interval23677|)
              (|_Make-Accessible23663| |object|
               |_Tangible-Entity23659|)
              (|_Be-Supported23661| |actions| |_Be-Supported23661|)
              (|_Be-Supported23661| |primitive-actions|
               |_Be-Supported23661|)
              (|_Be-Supported23661| |time-during|
               |_Time-Interval23670|)
              (|_Be-Supported23661| |object| |_Entity23666|)
              (|_Support23658| |actions| |_Support23658|)
              (|_Support23658| |preparatory-event| |_Move23664|)
              (|_Support23658| |preparatory-event|
               |_Make-Accessible23663|)
              (|_Support23658| |primitive-actions| |_Support23658|)
              (|_Support23658| |time-during| |_Time-Interval23662|)
              (|_Support23658| |object| |_Tangible-Entity23659|)
              (|_Support23658| |resulting-state| |_Be-Supported23661|))
             T NIL)))
(|Intensity-Scale| (|_Intensity-Scale37572|
                    (((|_Intensity-Scale37572| |instance-of|
                       |Intensity-Scale|)
                      (|_Number37574| |instance-of| |Number|)
                      (|_Intensity-Scale37572| |number-of-elements|
                       |_Number37574|))
                     T NIL)))
(|Bathroom| (|_Bathroom34367|
             (((|_Time-Interval34408| |instance-of| |Time-Interval|)
               (|_Time-Interval34405| |instance-of| |Time-Interval|)
               (|_Time-Interval34402| |instance-of| |Time-Interval|)
               (|_Time-Interval34397| |instance-of| |Time-Interval|)
               (|_Time-Interval34394| |instance-of| |Time-Interval|)
               (|_Time-Interval34391| |instance-of| |Time-Interval|)
               (|_Scalar34378| |instance-of| |Scalar|)
               (|_Be-Stable34382| |instance-of| |Be-Stable|)
               (|_Be-Supported34381| |instance-of| |Be-Supported|)
               (|_Cover34380| |instance-of| |Cover|)
               (|_Create34379| |instance-of| |Create|)
               (|_Angle-Value34377| |instance-of| |Angle-Value|)
               (|_Be-Stable34376| |instance-of| |Be-Stable|)
               (|_Be-Supported34375| |instance-of| |Be-Supported|)
               (|_Container34374| |instance-of| |Container|)
               (|_Create34373| |instance-of| |Create|)
               (|_Ceiling34372| |instance-of| |Ceiling|)
               (|_Wall34371| |instance-of| |Wall|)
               (|_Floor34370| |instance-of| |Floor|)
               (|_Bathroom34367| |instance-of| |Bathroom|)
               (|_Building34369| |instance-of| |Building|)
               (|_Be-Stable34376| |actions| |_Be-Stable34376|)
               (|_Be-Stable34376| |primitive-actions|
                |_Be-Stable34376|)
               (|_Be-Stable34376| |time-during| |_Time-Interval34408|)
               (|_Be-Supported34375| |actions| |_Be-Supported34375|)
               (|_Be-Supported34375| |primitive-actions|
                |_Be-Supported34375|)
               (|_Be-Supported34375| |time-during|
                |_Time-Interval34405|)
               (|_Create34373| |actions| |_Create34373|)
               (|_Create34373| |primitive-actions| |_Create34373|)
               (|_Create34373| |time-during| |_Time-Interval34402|)
               (|_Be-Stable34382| |actions| |_Be-Stable34382|)
               (|_Be-Stable34382| |primitive-actions|
                |_Be-Stable34382|)
               (|_Be-Stable34382| |time-during| |_Time-Interval34397|)
               (|_Be-Supported34381| |actions| |_Be-Supported34381|)
               (|_Be-Supported34381| |primitive-actions|
                |_Be-Supported34381|)
               (|_Be-Supported34381| |time-during|
                |_Time-Interval34394|)
               (|_Create34379| |actions| |_Create34379|)
               (|_Create34379| |primitive-actions| |_Create34379|)
               (|_Create34379| |time-during| |_Time-Interval34391|)
               (|_Angle-Value34377| |scalar-value| |_Scalar34378|)
               (|_Ceiling34372| |is-above| |_Bathroom34367|)
               (|_Ceiling34372| |object-of| |_Be-Stable34382|)
               (|_Ceiling34372| |object-of| |_Be-Supported34381|)
               (|_Ceiling34372| |plays| |_Cover34380|)
               (|_Ceiling34372| |result-of| |_Create34379|)
               (|_Ceiling34372| |orientation| |_Angle-Value34377|)
               (|_Bathroom34367| |object-of| |_Be-Stable34376|)
               (|_Bathroom34367| |object-of| |_Be-Supported34375|)
               (|_Bathroom34367| |plays| |_Container34374|)
               (|_Bathroom34367| |result-of| |_Create34373|)
               (|_Bathroom34367| |has-part| |_Ceiling34372|)
               (|_Bathroom34367| |has-part| |_Wall34371|)
               (|_Bathroom34367| |has-part| |_Floor34370|)
               (|_Bathroom34367| |is-part-of| |_Building34369|))
              T NIL)))
(|Penetrate| (|_Penetrate10090|
              (((|_Barrier10233| |instance-of| |Barrier|)
                (|_Tangible-Entity10152| |instance-of|
                 |Tangible-Entity|)
                (|_Tangible-Entity10151| |instance-of|
                 |Tangible-Entity|)
                (|_Time-Interval10131| |instance-of| |Time-Interval|)
                (|_Physical-Object10126| |instance-of|
                 |Physical-Object|)
                (|_Move10125| |instance-of| |Move|)
                (|_Time-Interval10124| |instance-of| |Time-Interval|)
                (|_Be-Broken10122| |instance-of| |Be-Broken|)
                (|_Portal10104| |instance-of| |Portal|)
                (|_Barrier10107| |instance-of| |Barrier|)
                (|_Move10109| |instance-of| |Move|)
                (|_Time-Interval10108| |instance-of| |Time-Interval|)
                (|_Spatial-Entity10103| |instance-of| |Spatial-Entity|)
                (|_Tangible-Entity10106| |instance-of|
                 |Physical-Object|)
                (|_Go-Through10102| |instance-of| |Go-Through|)
                (|_Breach10101| |instance-of| |Breach|)
                (|_Penetrate10090| |instance-of| |Penetrate|)
                (|_Tangible-Entity10100| |instance-of|
                 |Tangible-Entity|)
                (|_Tangible-Entity10152| |base-of| |_Go-Through10102|)
                (|_Tangible-Entity10152| |plays| |_Barrier10233|)
                (|_Spatial-Entity10103| |path-of| |_Go-Through10102|)
                (|_Spatial-Entity10103| |is-region-of|
                 |_Tangible-Entity10152|)
                (|_Breach10101| |object| |_Physical-Object10126|)
                (|_Breach10101| |agent| |_Tangible-Entity10151|)
                (|_Be-Broken10122| |actions| |_Be-Broken10122|)
                (|_Be-Broken10122| |primitive-actions|
                 |_Be-Broken10122|)
                (|_Be-Broken10122| |time-during| |_Time-Interval10131|)
                (|_Be-Broken10122| |object| |_Physical-Object10126|)
                (|_Breach10101| |actions-of| |_Breach10101|)
                (|_Breach10101| |preparatory-event| |_Move10125|)
                (|_Breach10101| |primitive-actions-of| |_Breach10101|)
                (|_Breach10101| |time-during| |_Time-Interval10124|)
                (|_Breach10101| |object| |_Tangible-Entity10106|)
                (|_Breach10101| |next-event| |_Go-Through10102|)
                (|_Breach10101| |resulting-state| |_Be-Broken10122|)
                (|_Breach10101| |agent| |_Tangible-Entity10100|)
                (|_Spatial-Entity10103| |plays| |_Portal10104|)
                (|_Spatial-Entity10103| |result-of| |_Breach10101|)
                (|_Tangible-Entity10106| |plays| |_Barrier10107|)
                (|_Penetrate10090| |actions| |_Penetrate10090|)
                (|_Penetrate10090| |actions| |_Go-Through10102|)
                (|_Penetrate10090| |actions| |_Breach10101|)
                (|_Penetrate10090| |all-subevents| |_Go-Through10102|)
                (|_Penetrate10090| |all-subevents| |_Breach10101|)
                (|_Penetrate10090| |preparatory-event| |_Move10109|)
                (|_Penetrate10090| |primitive-actions|
                 |_Go-Through10102|)
                (|_Penetrate10090| |primitive-actions| |_Breach10101|)
                (|_Penetrate10090| |time-during| |_Time-Interval10108|)
                (|_Penetrate10090| |path| |_Spatial-Entity10103|)
                (|_Penetrate10090| |object| |_Tangible-Entity10106|)
                (|_Penetrate10090| |first-subevent| |_Breach10101|)
                (|_Penetrate10090| |subevent| |_Go-Through10102|)
                (|_Penetrate10090| |subevent| |_Breach10101|)
                (|_Penetrate10090| |agent| |_Tangible-Entity10100|))
               T NIL)))
(|Perimeter-Scale| (|_Perimeter-Scale37557|
                    (((|_Perimeter-Scale37557| |instance-of|
                       |Perimeter-Scale|)
                      (|_Number37559| |instance-of| |Number|)
                      (|_Perimeter-Scale37557| |number-of-elements|
                       |_Number37559|))
                     T NIL)))
(|Breakability-Scale| (|_Breakability-Scale37642|
                       (((|_Breakability-Scale37642| |instance-of|
                          |Breakability-Scale|)
                         (|_Number37644| |instance-of| |Number|)
                         (|_Breakability-Scale37642|
                          |number-of-elements| |_Number37644|))
                        T NIL)))
(|Brunch| (|_Brunch681|
           (((|_Time-Interval688| |instance-of| |Time-Interval|)
             (|_Brunch681| |instance-of| |Brunch|)
             (|_Eat687| |instance-of| |Eat|)
             (|_Brunch681| |actions| |_Brunch681|)
             (|_Brunch681| |actions| |_Eat687|)
             (|_Brunch681| |all-subevents| |_Eat687|)
             (|_Brunch681| |primitive-actions| |_Eat687|)
             (|_Brunch681| |time-during| |_Time-Interval688|)
             (|_Brunch681| |subevent| |_Eat687|))
            T NIL)))
(|Condition-Node| (|_Condition-Node34900|
                   (((|_Condition-Node34900| |instance-of|
                      |Condition-Node|))
                    T NIL)))
(|Partition| (|_Partition37975|
              (((|_Partition37975| |instance-of| |Partition|)) T NIL)))
(|Head-Shake| (|_Head-Shake25727|
               (((|_Agent-Role25742| |instance-of| |Agent-Role|)
                 (|_Move25741| |instance-of| |Move|)
                 (|_Time-Interval25740| |instance-of| |Time-Interval|)
                 (|_Message25737| |instance-of| |Message|)
                 (|_Message25737| |instance-of| |Tangible-Entity|)
                 (|_Light25738| |instance-of| |Light|)
                 (|_Animal25734| |instance-of| |Animal|)
                 (|_Head-Shake25727| |instance-of| |Head-Shake|)
                 (|_Tangible-Entity25735| |instance-of|
                  |Tangible-Entity|)
                 (|_Animal25734| |capability| |_Agent-Role25742|)
                 (|_Tangible-Entity25735| |abuts| |_Animal25734|)
                 (|_Head-Shake25727| |actions| |_Head-Shake25727|)
                 (|_Head-Shake25727| |preparatory-event| |_Move25741|)
                 (|_Head-Shake25727| |primitive-actions|
                  |_Head-Shake25727|)
                 (|_Head-Shake25727| |time-during|
                  |_Time-Interval25740|)
                 (|_Head-Shake25727| |object| |_Message25737|)
                 (|_Head-Shake25727| |result| |_Light25738|)
                 (|_Head-Shake25727| |agent| |_Animal25734|)
                 (|_Head-Shake25727| |instrument|
                  |_Tangible-Entity25735|))
                T NIL)))
(|Hear| (|_Hear25536|
         (((|_Move25546| |instance-of| |Move|)
           (|_Time-Interval25545| |instance-of| |Time-Interval|)
           (|_Tangible-Entity25544| |instance-of| |Tangible-Entity|)
           (|_Sound25541| |instance-of| |Sound|)
           (|_Hear25536| |instance-of| |Hear|)
           (|_Message25543| |instance-of| |Message|)
           (|_Hear25536| |actions| |_Hear25536|)
           (|_Hear25536| |preparatory-event| |_Move25546|)
           (|_Hear25536| |primitive-actions| |_Hear25536|)
           (|_Hear25536| |time-during| |_Time-Interval25545|)
           (|_Hear25536| |experiencer| |_Tangible-Entity25544|)
           (|_Hear25536| |object| |_Sound25541|)
           (|_Hear25536| |result| |_Message25543|))
          T NIL)))
(|State| (|_State27|
          (((|_State27| |instance-of| |State|)
            (|_Time-Interval29| |instance-of| |Time-Interval|)
            (|_State27| |actions| |_State27|)
            (|_State27| |primitive-actions| |_State27|)
            (|_State27| |time-during| |_Time-Interval29|))
           T NIL)))
(|Session| (|_Session785|
            (((|_Session785| |instance-of| |Session|)
              (|_Time-Interval791| |instance-of| |Time-Interval|)
              (|_Session785| |actions| |_Session785|)
              (|_Session785| |primitive-actions| |_Session785|)
              (|_Session785| |time-during| |_Time-Interval791|))
             T NIL)))
(|Service| (|_Service442|
            (((|_Time-Interval450| |instance-of| |Time-Interval|)
              (|_Entity448| |instance-of| |Entity|)
              (|_Service442| |instance-of| |Service|)
              (|_Entity447| |instance-of| |Entity|)
              (|_Service442| |actions| |_Service442|)
              (|_Service442| |primitive-actions| |_Service442|)
              (|_Service442| |time-during| |_Time-Interval450|)
              (|_Service442| |recipient| |_Entity448|)
              (|_Service442| |donor| |_Entity447|))
             T NIL)))
(|Temperature-Value| (|_Temperature-Value37692|
                      (((|_Temperature-Value37692| |instance-of|
                         |Temperature-Value|))
                       T NIL)))
(|Temporal-Relation| (|_Temporal-Relation37941|
                      (((|_Temporal-Relation37941| |instance-of|
                         |Temporal-Relation|))
                       T NIL)))
(|Perimeter-Constant| (|_Perimeter-Constant37857|
                       (((|_Perimeter-Constant37857| |instance-of|
                          |Perimeter-Constant|))
                        T NIL)))
(|Age-Constant| (|_Age-Constant37921|
                 (((|_Age-Constant37921| |instance-of| |Age-Constant|))
                  T NIL)))
(|Bag-Aggregation-Slot| (|_Bag-Aggregation-Slot20|
                         (((|_Bag-Aggregation-Slot20| |instance-of|
                            |Bag-Aggregation-Slot|))
                          T NIL)))
(|Gas-Substance| (|_Gas-Substance33921|
                  (((|_Categorical33926| |instance-of| |Categorical|)
                    (|_Gas-Substance33921| |instance-of|
                     |Gas-Substance|)
                    (|_State-Value33925| |instance-of| |State-Value|)
                    (|_State-Value33925| |categorical-value|
                     |_Categorical33926|)
                    (|_Gas-Substance33921| |physical-state|
                     |_State-Value33925|))
                   T NIL)))
(|SHAKEN-Column-Content-Order-Constant| (|_SHAKEN-Column-Content-Order-Constant37981|
                                         (((|_SHAKEN-Column-Content-Order-Constant37981|
                                            |instance-of|
                                            |SHAKEN-Column-Content-Order-Constant|))
                                          T
                                          NIL)))
(|Viewpoint-Query-Type| (|_Viewpoint-Query-Type34938|
                         (((|_Viewpoint-Query-Type34938| |instance-of|
                            |Viewpoint-Query-Type|))
                          T NIL)))
(|Density-Constant| (|_Density-Constant37899|
                     (((|_Density-Constant37899| |instance-of|
                        |Density-Constant|))
                      T NIL)))
(|Intentional| (|_Intentional37330|
                (((|_Intentional37330| |instance-of| |Intentional|)) T
                 NIL)))
(|Absorb| (|_Absorb20490|
           (((|_Container20621| |instance-of| |Container|)
             (|_Time-Interval20620| |instance-of| |Time-Interval|)
             (|_Tangible-Entity20614| |instance-of| |Tangible-Entity|)
             (|_Tangible-Entity20616| |instance-of| |Fluid-Substance|)
             (|_Move20613| |instance-of| |Move|)
             (|_Admit20611| |instance-of| |Admit|)
             (|_Time-Interval20610| |instance-of| |Time-Interval|)
             (|_Be-Shut-Out20609| |instance-of| |Be-Shut-Out|)
             (|_Move20598| |instance-of| |Move|)
             (|_Shut-Out20596| |instance-of| |Shut-Out|)
             (|_Time-Interval20595| |instance-of| |Time-Interval|)
             (|_Portal20517| |instance-of| |Portal|)
             (|_Container20512| |instance-of| |Container|)
             (|_Time-Interval20569| |instance-of| |Time-Interval|)
             (|_Spatial-Entity20564| |instance-of| |Spatial-Entity|)
             (|_Spatial-Entity20563| |instance-of| |Spatial-Entity|)
             (|_Tangible-Entity20568| |instance-of| |Tangible-Entity|)
             (|_Tangible-Entity20561| |instance-of| |Fluid-Substance|)
             (|_Move20548| |instance-of| |Move|)
             (|_Move20547| |instance-of| |Move|)
             (|_Admit20546| |instance-of| |Admit|)
             (|_Time-Interval20545| |instance-of| |Time-Interval|)
             (|_Spatial-Entity20514| |instance-of| |Spatial-Entity|)
             (|_Spatial-Entity20513| |instance-of| |Spatial-Entity|)
             (|_Spatial-Entity20515| |instance-of| |Spatial-Entity|)
             (|_Fluid-Substance20529| |instance-of| |Fluid-Substance|)
             (|_Be-Contained20544| |instance-of| |Be-Contained|)
             (|_Tangible-Entity20511| |instance-of| |Tangible-Entity|)
             (|_Acceleration-Magnitude-Value20520| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Acceleration-Vector-Value20519| |instance-of|
              |Acceleration-Vector-Value|)
             (|_Length-Value20526| |instance-of| |Length-Value|)
             (|_Duration-Value20518| |instance-of| |Duration-Value|)
             (|_Speed-Value20522| |instance-of| |Speed-Value|)
             (|_Displacement-Vector-Value20525| |instance-of|
              |Displacement-Vector-Value|)
             (|_Speed-Value20543| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value20540| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value20542| |instance-of| |Speed-Value|)
             (|_Speed-Value20541| |instance-of| |Speed-Value|)
             (|_Speed-Value20539| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value20536| |instance-of|
              |Velocity-Vector-Value|)
             (|_Speed-Value20538| |instance-of| |Speed-Value|)
             (|_Speed-Value20537| |instance-of| |Speed-Value|)
             (|_Velocity-Vector-Value20521| |instance-of|
              |Velocity-Vector-Value|)
             (|_Acceleration-Magnitude-Value20535| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value20534| |instance-of| |Length-Value|)
             (|_Speed-Value20533| |instance-of| |Speed-Value|)
             (|_Acceleration-Magnitude-Value20532| |instance-of|
              |Acceleration-Magnitude-Value|)
             (|_Length-Value20531| |instance-of| |Length-Value|)
             (|_Absorb20490| |instance-of| |Absorb|)
             (|_Speed-Value20530| |instance-of| |Speed-Value|)
             (|_Tangible-Entity20614| |plays| |_Container20621|)
             (|_Tangible-Entity20616| |is-outside|
              |_Tangible-Entity20614|)
             (|_Be-Shut-Out20609| |actions| |_Be-Shut-Out20609|)
             (|_Be-Shut-Out20609| |primitive-actions|
              |_Be-Shut-Out20609|)
             (|_Be-Shut-Out20609| |time-during| |_Time-Interval20620|)
             (|_Be-Shut-Out20609| |base| |_Tangible-Entity20614|)
             (|_Be-Shut-Out20609| |in-event-of|
              |_Tangible-Entity20614|)
             (|_Be-Shut-Out20609| |object| |_Tangible-Entity20616|)
             (|_Shut-Out20596| |actions| |_Shut-Out20596|)
             (|_Shut-Out20596| |preparatory-event| |_Move20613|)
             (|_Shut-Out20596| |preparatory-event| |_Admit20611|)
             (|_Shut-Out20596| |primitive-actions| |_Shut-Out20596|)
             (|_Shut-Out20596| |time-during| |_Time-Interval20610|)
             (|_Shut-Out20596| |base| |_Tangible-Entity20511|)
             (|_Shut-Out20596| |object| |_Fluid-Substance20529|)
             (|_Shut-Out20596| |resulting-state| |_Be-Shut-Out20609|)
             (|_Be-Contained20544| |object| |_Fluid-Substance20529|)
             (|_Admit20546| |actions| |_Admit20546|)
             (|_Admit20546| |preparatory-event| |_Move20598|)
             (|_Admit20546| |preparatory-event| |_Shut-Out20596|)
             (|_Admit20546| |primitive-actions| |_Admit20546|)
             (|_Admit20546| |time-during| |_Time-Interval20595|)
             (|_Admit20546| |base| |_Tangible-Entity20511|)
             (|_Admit20546| |object| |_Fluid-Substance20529|)
             (|_Spatial-Entity20513| |is-outside|
              |_Tangible-Entity20511|)
             (|_Tangible-Entity20511| |base-of| |_Be-Contained20544|)
             (|_Spatial-Entity20515| |plays| |_Portal20517|)
             (|_Tangible-Entity20511| |encloses|
              |_Spatial-Entity20514|)
             (|_Tangible-Entity20511| |plays| |_Container20512|)
             (|_Tangible-Entity20511| |has-region|
              |_Spatial-Entity20515|)
             (|_Be-Contained20544| |actions| |_Be-Contained20544|)
             (|_Be-Contained20544| |primitive-actions|
              |_Be-Contained20544|)
             (|_Be-Contained20544| |time-during| |_Time-Interval20569|)
             (|_Be-Contained20544| |destination|
              |_Spatial-Entity20564|)
             (|_Be-Contained20544| |origin| |_Spatial-Entity20563|)
             (|_Be-Contained20544| |base| |_Tangible-Entity20568|)
             (|_Be-Contained20544| |object| |_Tangible-Entity20561|)
             (|_Absorb20490| |actions| |_Absorb20490|)
             (|_Absorb20490| |preparatory-event| |_Move20548|)
             (|_Absorb20490| |preparatory-event| |_Move20547|)
             (|_Absorb20490| |preparatory-event| |_Admit20546|)
             (|_Absorb20490| |primitive-actions| |_Absorb20490|)
             (|_Absorb20490| |time-during| |_Time-Interval20545|)
             (|_Absorb20490| |destination| |_Spatial-Entity20514|)
             (|_Absorb20490| |origin| |_Spatial-Entity20513|)
             (|_Absorb20490| |path| |_Spatial-Entity20515|)
             (|_Absorb20490| |base| |_Tangible-Entity20511|)
             (|_Absorb20490| |object| |_Fluid-Substance20529|)
             (|_Absorb20490| |resulting-state| |_Be-Contained20544|)
             (|_Absorb20490| |agent| |_Tangible-Entity20511|)
             (|_Absorb20490| |acceleration-magnitude|
              |_Acceleration-Magnitude-Value20520|)
             (|_Absorb20490| |acceleration|
              |_Acceleration-Vector-Value20519|)
             (|_Absorb20490| |distance| |_Length-Value20526|)
             (|_Absorb20490| |duration| |_Duration-Value20518|)
             (|_Absorb20490| |speed| |_Speed-Value20522|)
             (|_Absorb20490| |displacement|
              |_Displacement-Vector-Value20525|)
             (|_Absorb20490| |final-speed| |_Speed-Value20543|)
             (|_Absorb20490| |final-velocity|
              |_Velocity-Vector-Value20540|)
             (|_Absorb20490| |final-x-speed| |_Speed-Value20542|)
             (|_Absorb20490| |final-y-speed| |_Speed-Value20541|)
             (|_Absorb20490| |initial-speed| |_Speed-Value20539|)
             (|_Absorb20490| |initial-velocity|
              |_Velocity-Vector-Value20536|)
             (|_Absorb20490| |initial-x-speed| |_Speed-Value20538|)
             (|_Absorb20490| |initial-y-speed| |_Speed-Value20537|)
             (|_Absorb20490| |velocity| |_Velocity-Vector-Value20521|)
             (|_Absorb20490| |x-acceleration-magnitude|
              |_Acceleration-Magnitude-Value20535|)
             (|_Absorb20490| |x-distance| |_Length-Value20534|)
             (|_Absorb20490| |x-speed| |_Speed-Value20533|)
             (|_Absorb20490| |y-acceleration-magnitude|
              |_Acceleration-Magnitude-Value20532|)
             (|_Absorb20490| |y-distance| |_Length-Value20531|)
             (|_Absorb20490| |y-speed| |_Speed-Value20530|))
            T NIL)))
(|Leave| (|_Leave35099|
          (((|_Go-To35128| |instance-of| |Go-To|)
            (|_Time-Interval35127| |instance-of| |Time-Interval|)
            (|_Spatial-Entity35101| |instance-of| |Spatial-Entity|)
            (|_Tangible-Entity35100| |instance-of| |Tangible-Entity|)
            (|_Acceleration-Magnitude-Value35104| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Acceleration-Vector-Value35103| |instance-of|
             |Acceleration-Vector-Value|)
            (|_Length-Value35110| |instance-of| |Length-Value|)
            (|_Duration-Value35102| |instance-of| |Duration-Value|)
            (|_Speed-Value35106| |instance-of| |Speed-Value|)
            (|_Displacement-Vector-Value35109| |instance-of|
             |Displacement-Vector-Value|)
            (|_Speed-Value35126| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value35123| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value35125| |instance-of| |Speed-Value|)
            (|_Speed-Value35124| |instance-of| |Speed-Value|)
            (|_Speed-Value35122| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value35119| |instance-of|
             |Velocity-Vector-Value|)
            (|_Speed-Value35121| |instance-of| |Speed-Value|)
            (|_Speed-Value35120| |instance-of| |Speed-Value|)
            (|_Velocity-Vector-Value35105| |instance-of|
             |Velocity-Vector-Value|)
            (|_Acceleration-Magnitude-Value35118| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value35117| |instance-of| |Length-Value|)
            (|_Speed-Value35116| |instance-of| |Speed-Value|)
            (|_Acceleration-Magnitude-Value35115| |instance-of|
             |Acceleration-Magnitude-Value|)
            (|_Length-Value35114| |instance-of| |Length-Value|)
            (|_Leave35099| |instance-of| |Leave|)
            (|_Speed-Value35113| |instance-of| |Speed-Value|)
            (|_Leave35099| |actions| |_Leave35099|)
            (|_Leave35099| |preparatory-event| |_Go-To35128|)
            (|_Leave35099| |primitive-actions| |_Leave35099|)
            (|_Leave35099| |time-during| |_Time-Interval35127|)
            (|_Leave35099| |origin| |_Spatial-Entity35101|)
            (|_Leave35099| |object| |_Tangible-Entity35100|)
            (|_Leave35099| |agent| |_Tangible-Entity35100|)
            (|_Leave35099| |acceleration-magnitude|
             |_Acceleration-Magnitude-Value35104|)
            (|_Leave35099| |acceleration|
             |_Acceleration-Vector-Value35103|)
            (|_Leave35099| |distance| |_Length-Value35110|)
            (|_Leave35099| |duration| |_Duration-Value35102|)
            (|_Leave35099| |speed| |_Speed-Value35106|)
            (|_Leave35099| |displacement|
             |_Displacement-Vector-Value35109|)
            (|_Leave35099| |final-speed| |_Speed-Value35126|)
            (|_Leave35099| |final-velocity|
             |_Velocity-Vector-Value35123|)
            (|_Leave35099| |final-x-speed| |_Speed-Value35125|)
            (|_Leave35099| |final-y-speed| |_Speed-Value35124|)
            (|_Leave35099| |initial-speed| |_Speed-Value35122|)
            (|_Leave35099| |initial-velocity|
             |_Velocity-Vector-Value35119|)
            (|_Leave35099| |initial-x-speed| |_Speed-Value35121|)
            (|_Leave35099| |initial-y-speed| |_Speed-Value35120|)
            (|_Leave35099| |velocity| |_Velocity-Vector-Value35105|)
            (|_Leave35099| |x-acceleration-magnitude|
             |_Acceleration-Magnitude-Value35118|)
            (|_Leave35099| |x-distance| |_Length-Value35117|)
            (|_Leave35099| |x-speed| |_Speed-Value35116|)
            (|_Leave35099| |y-acceleration-magnitude|
             |_Acceleration-Magnitude-Value35115|)
            (|_Leave35099| |y-distance| |_Length-Value35114|)
            (|_Leave35099| |y-speed| |_Speed-Value35113|))
           T NIL)))
(|UoM-Momentum| (|_UoM-Momentum37448|
                 (((|_UoM-Momentum37448| |instance-of| |UoM-Momentum|))
                  T NIL)))
(|Transfer| (|_Transfer983|
             (((|_Move991| |instance-of| |Move|)
               (|_Obtain990| |instance-of| |Obtain|)
               (|_Time-Interval989| |instance-of| |Time-Interval|)
               (|_Transfer983| |instance-of| |Transfer|)
               (|_Entity987| |instance-of| |Spatial-Entity|)
               (|_Transfer983| |actions| |_Transfer983|)
               (|_Transfer983| |preparatory-event| |_Move991|)
               (|_Transfer983| |preparatory-event| |_Obtain990|)
               (|_Transfer983| |primitive-actions| |_Transfer983|)
               (|_Transfer983| |time-during| |_Time-Interval989|)
               (|_Transfer983| |object| |_Entity987|))
              T NIL)))
(|UoM-Area| (|_UoM-Area37474|
             (((|_UoM-Area37474| |instance-of| |UoM-Area|)) T NIL)))
(|Group-Node| (|_Group-Node34902|
               (((|_Group-Node34902| |instance-of| |Group-Node|)) T
                NIL)))
(|Sequence| (|_Sequence34726|
             (((|_Sequence34726| |instance-of| |Sequence|)) T NIL)))
(|River| (|_River34623|
          (((|_River34623| |instance-of| |River|)) T NIL)))
(|Truth-Value| (|_Truth-Value37686|
                (((|_Truth-Value37686| |instance-of| |Truth-Value|)) T
                 NIL)))
(|UoM-Frequency| (|_UoM-Frequency37464|
                  (((|_UoM-Frequency37464| |instance-of|
                     |UoM-Frequency|))
                   T NIL)))
(|Activity| (|_Activity415|
             (((|_Activity415| |instance-of| |Activity|)
               (|_Time-Interval421| |instance-of| |Time-Interval|)
               (|_Activity415| |actions| |_Activity415|)
               (|_Activity415| |primitive-actions| |_Activity415|)
               (|_Activity415| |time-during| |_Time-Interval421|))
              T NIL)))
(|Be-Sitting| (|_Be-Sitting224|
               (((|_Time-Interval228| |instance-of| |Time-Interval|)
                 (|_Be-Sitting224| |instance-of| |Be-Sitting|)
                 (|_Entity227| |instance-of| |Entity|)
                 (|_Be-Sitting224| |actions| |_Be-Sitting224|)
                 (|_Be-Sitting224| |primitive-actions|
                  |_Be-Sitting224|)
                 (|_Be-Sitting224| |time-during| |_Time-Interval228|)
                 (|_Be-Sitting224| |object| |_Entity227|))
                T NIL)))
(|Animacy-Constant| (|_Animacy-Constant37917|
                     (((|_Animacy-Constant37917| |instance-of|
                        |Animacy-Constant|))
                      T NIL)))
(|Sense| (|_Sense25456|
          (((|_Move25466| |instance-of| |Move|)
            (|_Time-Interval25465| |instance-of| |Time-Interval|)
            (|_Tangible-Entity25464| |instance-of| |Tangible-Entity|)
            (|_Tangible-Entity25461| |instance-of| |Tangible-Entity|)
            (|_Sense25456| |instance-of| |Sense|)
            (|_Message25463| |instance-of| |Message|)
            (|_Sense25456| |actions| |_Sense25456|)
            (|_Sense25456| |preparatory-event| |_Move25466|)
            (|_Sense25456| |primitive-actions| |_Sense25456|)
            (|_Sense25456| |time-during| |_Time-Interval25465|)
            (|_Sense25456| |experiencer| |_Tangible-Entity25464|)
            (|_Sense25456| |object| |_Tangible-Entity25461|)
            (|_Sense25456| |result| |_Message25463|))
           T NIL)))
(|Multislot-Value-Viewpoint| (|_Multislot-Value-Viewpoint34930|
                              (((|_Multislot-Value-Viewpoint34930|
                                 |instance-of|
                                 |Multislot-Value-Viewpoint|))
                               T NIL)))
(|Smell-Constant| (|_Smell-Constant37827|
                   (((|_Smell-Constant37827| |instance-of|
                      |Smell-Constant|))
                    T NIL)))
(|Donate| (|_Donate1801|
           (((|_Move1822| |instance-of| |Move|)
             (|_Obtain1821| |instance-of| |Obtain|)
             (|_Time-Interval1820| |instance-of| |Time-Interval|)
             (|_Move1815| |instance-of| |Move|)
             (|_Obtain1814| |instance-of| |Obtain|)
             (|_Time-Interval1813| |instance-of| |Time-Interval|)
             (|_Tangible-Entity1810| |instance-of| |Living-Entity|)
             (|_Entity1812| |instance-of| |Spatial-Entity|)
             (|_Tangible-Entity1809| |instance-of| |Tangible-Entity|)
             (|_Donate1801| |instance-of| |Donate|)
             (|_Tangible-Entity1808| |instance-of| |Tangible-Entity|)
             (|_Obtain1814| |actions| |_Obtain1814|)
             (|_Obtain1814| |preparatory-event| |_Move1822|)
             (|_Obtain1814| |preparatory-event| |_Obtain1821|)
             (|_Obtain1814| |primitive-actions| |_Obtain1814|)
             (|_Obtain1814| |time-during| |_Time-Interval1820|)
             (|_Obtain1814| |object| |_Entity1812|)
             (|_Obtain1814| |recipient| |_Tangible-Entity1808|)
             (|_Obtain1814| |agent| |_Tangible-Entity1808|)
             (|_Donate1801| |actions| |_Donate1801|)
             (|_Donate1801| |preparatory-event| |_Move1815|)
             (|_Donate1801| |preparatory-event| |_Obtain1814|)
             (|_Donate1801| |primitive-actions| |_Donate1801|)
             (|_Donate1801| |time-during| |_Time-Interval1813|)
             (|_Donate1801| |beneficiary| |_Tangible-Entity1810|)
             (|_Donate1801| |object| |_Entity1812|)
             (|_Donate1801| |recipient| |_Tangible-Entity1809|)
             (|_Donate1801| |agent| |_Tangible-Entity1808|)
             (|_Donate1801| |donor| |_Tangible-Entity1808|))
            T NIL)))
))
