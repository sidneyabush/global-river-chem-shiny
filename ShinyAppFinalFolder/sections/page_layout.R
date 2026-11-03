# ---- user interface ----
overview_ui <- navbarPage(
  title = NULL,
  id = "app_tab",
  windowTitle = "Global River Chemistry",
  theme = bslib::bs_theme(version = 5, primary = app_blue, secondary = app_teal,
    fg = app_ink, bg = "#f4f7f6", base_font = "Arial"),

  header = tags$head(tags$style(HTML("
    body { background: #f4f7f6; color: #24323d; font-family: Arial, sans-serif; }
    .leaflet-container, .leaflet-control, .leaflet-popup, .irs, .selectize-dropdown, .dataTables_wrapper {
      font-family: Arial, sans-serif; }
    .navbar { background: #fff !important; border-bottom: 1px solid #d7e3ea; }
    .navbar .nav-link { color: #334b59; }
    .navbar .nav-link.active { color: #174e68; background: #e8f2f3; border-radius: .55rem; }
    .navbar-brand:empty { display: none; }
    .tab-content > .tab-pane > .container-fluid, .workspace-stack, .project-page {
      max-width: 1320px; margin: auto; padding: clamp(1rem, 3vw, 2.5rem);
    }
    .project-hero { display: grid; grid-template-columns: minmax(0, 2fr) minmax(220px, 1fr);
      gap: 2rem; align-items: center; padding: clamp(1.5rem, 5vw, 4rem);
      border-radius: 24px; background: linear-gradient(130deg, #153a4b, #1b6261); color: white;
      position: relative; isolation: isolate; overflow: hidden; }
    .hero-diatom { position: absolute; z-index: -1; pointer-events: none;
      height: auto; opacity: .44; overflow: hidden; }
    .diatom-round-large { width: clamp(125px, 19vw, 230px); right: -38px; top: -48px; }
    .diatom-long { width: clamp(72px, 10vw, 125px); left: 44%; bottom: 7%; transform: rotate(-16deg); }
    .diatom-round-small { width: clamp(74px, 10vw, 128px); left: -35px; bottom: -30px; }
    .diatom-needle { width: clamp(45px, 6vw, 78px); top: 5%; left: 53%; transform: rotate(16deg); }
    .diatom-triangle { width: clamp(42px, 6vw, 78px); right: 6%; bottom: 6%; transform: rotate(17deg); }
    .diatom-disc { width: clamp(25px, 3.5vw, 43px); top: 12%; left: 65%; opacity: .36; }
    .diatom-mini { width: clamp(23px, 3vw, 37px); right: 2%; bottom: 31%; }
    .diatom-oval { width: clamp(42px, 5.5vw, 68px); left: 66%; bottom: 5%; transform: rotate(20deg); }
    .diatom-round-bottom { width: clamp(64px, 8vw, 104px); right: 16%; bottom: 4%; opacity: .3; }
    .diatom-left-triangle { width: clamp(55px, 7vw, 92px); left: -9px; top: -23px; transform: rotate(-15deg); }
    .diatom-long-small { width: clamp(35px, 4.5vw, 55px); left: 23%; bottom: 5%; transform: rotate(22deg); }
    .diatom-gold-disc { width: clamp(39px, 5.5vw, 70px); left: 37%; top: 4%; opacity: .33; }
    .diatom-left-disc { width: clamp(24px, 3.3vw, 42px); left: 2%; top: 48%; }
    .diatom-right-needle { width: clamp(36px, 4.5vw, 56px); right: 25%; top: 4%; transform: rotate(29deg); }
    .diatom-tiny-triangle { width: clamp(25px, 3.3vw, 42px); left: 58%; bottom: 6%; }
    .diatom-inner-triangle { width: clamp(37px, 5vw, 63px); left: 55%; top: 46%; opacity: .3; }
    .diatom-tiny-needle { width: clamp(32px, 4.2vw, 54px); left: 31%; bottom: 10%; }
    .project-hero h1 { font-size: clamp(2.35rem, 5vw, 4rem); font-weight: 760; letter-spacing: -.045em; }
    .project-intro > p { max-width: 57ch; }
    .paper-journal { font-size: .8rem; letter-spacing: .06em; font-weight: 700; }
    .project-button { display: inline-block; margin: .35rem .9rem .35rem 0; }
    .project-button { color: #123e50; background: #fff; border-radius: .55rem; padding: .7rem 1rem;
      font-weight: 700; text-decoration: none; }
    .project-stats { display: grid; gap: .65rem; }
    .project-stats div { padding: .75rem 1rem; border: 1px solid #ffffff50; border-radius: .75rem;
      background: #ffffff12; display: flex; align-items: baseline; gap: .75rem; flex-wrap: wrap; }
    .project-stats strong { font-size: 1.65rem; }
    .project-section { margin-top: 2rem; }
    .project-footer { margin-top: 2rem; padding: 1.5rem 0 0; border-top: 1px solid #d7e3ea; }
    .partner-grid { display: grid; gap: .65rem; margin: 1rem 0 1.4rem;
      grid-template-columns: repeat(3, minmax(0, 1fr)); }
    .partner-tile { display: grid; justify-items: center; gap: .7rem; padding: 1rem;
      border: 1px solid #d7e3ea; border-radius: 12px; background: white;
      text-align: center; font-size: .86rem; color: #425d69; }
    .partner-mark { display: grid; place-items: center; width: 100%; height: 78px; }
    .partner-mark img, .partner-mark svg { width: 100%; height: 100%; max-width: 190px;
      object-fit: contain; overflow: hidden; }
    .project-license { padding: .9rem 1rem; border-radius: .6rem; background: #e8f2f3; }
    .workshop-resources summary { cursor: pointer; font-weight: 650; }
    .paper-list, .team-grid { display: grid; gap: .8rem; padding: 0; }
    .paper-list { list-style: none; grid-template-columns: repeat(auto-fit, minmax(min(100%, 320px), 1fr)); }
    .paper-list li, .team-card, .card { background: #fff; border: 1px solid #d7e3ea; border-radius: 14px; }
    .paper-list li { padding: 1rem 1.2rem; }
    .paper-list a { display: block; font-weight: 650; margin-top: .25rem; }
    .paper-journal { color: #425d69; }
    .paper-authors { margin-top: .55rem; font-size: .9rem; color: #425d69; }
    .paper-authors summary { cursor: pointer; }
    .paper-authors p { margin: .4rem 0 0; }
    .team-grid { grid-template-columns: repeat(auto-fit, minmax(min(100%, 195px), 1fr)); }
    .team-card { padding: 1rem; }
    .team-card h3 { margin: .75rem 0 .2rem; font-size: 1.05rem; }
    .team-affiliation { margin: 0 0 .55rem; color: #526575; font-size: .9rem; }
    .profile-photo { display: grid; place-items: center; width: 66px; height: 66px; border-radius: 50%;
      overflow: hidden; background: #e2eeef; color: #17556b; font-weight: 700; }
    .profile-photo > * { grid-area: 1 / 1; }
    .profile-photo img { width: 100%; height: 100%; object-fit: cover; }
    .team-links { display: flex; flex-wrap: wrap; gap: .45rem; }
    .team-links a { padding: .2rem .5rem; background: #edf4f5; border-radius: .4rem; font-size: .86rem; }
    .workspace-stack { display: grid; gap: 1rem; }
    .workspace-stack > *, .output-card, .bslib-sidebar-layout > .main { min-width: 0; }
    .analysis-layout .sidebar { background: #edf4f5; }
    .analysis-layout .shiny-input-container { max-width: 100%; }
    .analysis-layout .action-button { white-space: normal; }
    .bslib-card .card-header { font-weight: 650; }
    .bslib-card .card-header { background: #edf4f5; border-color: #d7e3ea; }
    .nav-tabs .nav-link.active { color: #174e68; background: #e8f2f3; }
    table.dataTable thead th { background: #edf4f5; color: #24323d; }
    #table1, #table3 { overflow-x: auto; }
    #table2 td { vertical-align: top; }
    #table2 summary { cursor: pointer; color: #245878; }
    #table2 td details { min-width: 7rem; }
    #table2 .solute-coverage-detail { width: auto; border-collapse: collapse; }
    #table2 .solute-coverage-detail th, #table2 .solute-coverage-detail td { padding: .2rem .45rem; white-space: nowrap; }
    #table2 .solute-coverage-detail td:not(:first-child) { text-align: right; }
    #table1 tr.new-product > td { background-color: #e2eeef; box-shadow: none; }
    #plot1 img { width: 100%; height: 100%; object-fit: contain; }
    .map-with-controls { display: grid; grid-template-columns: minmax(175px, 230px) minmax(0, 1fr); gap: 1rem; }
    .map-with-legend { display: grid; grid-template-columns: minmax(0, 1fr) minmax(180px, 230px); gap: 1rem; }
    #availability_map { height: auto !important; aspect-ratio: 1; max-height: 500px; }
    .map-legend { max-height: 500px; overflow-y: auto; padding: .75rem; border-left: 1px solid #d7e3ea; }
    .map-legend ul { list-style: none; margin: 0; padding: 0; }
    .map-legend li { display: flex; align-items: baseline; gap: .45rem; margin: .2rem 0; }
    .map-legend-swatch { display: inline-block; flex: 0 0 .75rem; height: .75rem; border-radius: 50%; }
    .map-legend-gradient { display: flex; gap: .55rem; height: 10rem; }
    .map-legend-ramp { width: .75rem; border-radius: 3px; margin: .55rem 0; }
    .map-legend-values { display: flex; flex-direction: column; justify-content: space-between; }
    @media (max-width: 900px) {
      .map-with-controls, .map-with-legend { grid-template-columns: minmax(0, 1fr); }
      .map-legend { max-height: 230px; border-left: 0; border-top: 1px solid #d7e3ea; }
    }
    @media (max-width: 700px) {
      .project-hero { grid-template-columns: 1fr; }
      .partner-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
    }
    a.dataset-link { text-decoration: underline; }
  "))),

  # ---- project landing page ----
  tabPanel("About the Project",
    tags$main(class = "project-page",
      tags$section(class = "project-hero",
        diatom_drawing("1113 121 344 339", "diatom-round-large", paste(
          "M 1113.3 281.5 1113.1 305.8 1113.6 306.3 1114.1 315.4 1117.3 331.9 1122.1 347.4 1126.9 359.1",
          "1134.4 373.5 1144.5 389 1153.6 400.2 1169.3 415.9 1182.1 426 1194.4 434 1210.9 442.6",
          "1222.7 447.4 1239.7 452.7 1258.4 456.4 1272.3 457.5 1272.8 458 1296.3 458 1296.8 457.5",
          "1306.9 457 1321.9 454.3 1340.5 449 1353.9 443.6 1373.1 433.5 1386.4 424.4 1398.1 414.8",
          "1411.7 401.2 1424 385.8 1433.6 370.3 1440 357.5 1448 335.7 1451.2 323.4 1453.9 307.9",
          "1454.4 296.2 1454.9 295.7 1454.9 279.1 1454.4 278.6 1453.3 263.7 1449.6 246.6 1444.8 232.2",
          "1435.7 213 1430.9 205 1422.4 192.8 1411.2 179.4 1396 164.8 1385.3 156.2 1372 147.2",
          "1353.3 137 1337.9 130.6 1324.5 126.4 1307.5 122.6 1294.7 121.6 1294.1 121 1271.2 121",
          "1270.7 121.6 1261.6 122.1 1245.6 125.3 1228.5 130.6 1219.5 134.4 1198.1 145.6 1186.9 153",
          "1170.9 165.8 1156.8 180 1145.6 193.8 1136.5 207.7 1127.5 225.3 1121.6 240.2 1118.4 250.9",
          "1114.1 271.7 1113.3 281.5 Z")),
        diatom_drawing("587 1077 296 274", "diatom-long", paste(
          "M 588 1121.9 587.7 1131.8 588.3 1132.3 588.8 1140.8 590.9 1151 594.7 1163.2 600 1176.5",
          "606.4 1189.3 616.5 1205.9 636.3 1231.5 661.9 1259.7 688.8 1285.6 716.5 1308 740 1323.4",
          "762.4 1335.2 780 1342.1 796 1346.4 798.7 1346.4 805.1 1348 823.7 1349 824.3 1348.5",
          "834.9 1348 841.9 1346.4 849.3 1343.7 857.9 1338.9 862.7 1335.2 870.4 1326.9 876.8 1315.7",
          "879.5 1307.7 880.5 1297.1 881.1 1296.5 881.1 1288 880.5 1287.5 880 1277.9 876.3 1261.9",
          "870.9 1246.9 862.4 1229.3 854.4 1216 842.7 1199.5 828.3 1181.9 817.1 1169.6 791.7 1145.4",
          "778.9 1134.7 764 1123.5 735.7 1105.4 708 1091 695.2 1085.6 682.4 1081.4 668.5 1078.2",
          "660.5 1077.6 660 1077.1 641.3 1077.1 631.2 1078.7 621.6 1081.4 608.3 1087.8 600.3 1094.2",
          "596.8 1097.6 593.1 1103 588.8 1114.2 588 1121.9 Z")),
        diatom_drawing("297 87 267 269", "diatom-round-small", paste(
          "M 297.3 210.6 297.1 232.2 297.6 232.7 297.6 237 299.2 247.1 302.4 259.9 307.7 274.3",
          "314.7 288.2 321.6 298.9 329.1 308.5 345.9 324.7 356.5 332.7 366.1 338.6 377.9 344.5",
          "390.7 349.3 405.6 353 417.3 354.1 417.9 354.6 434.4 354.6 434.9 354.1 443.5 353.5",
          "454.7 351.4 468.5 347.1 476.5 343.9 490.4 337 503.7 328.5 513.9 320.5 525.9 309",
          "532.8 301 538.1 294.1 547.2 279.7 551.5 271.1 556.8 257.3 559.5 247.1 561.6 233.3",
          "561.6 212 559.5 197.6 555.2 181.6 550.4 169.3 542.9 154.9 536 144.2 527.5 133.6",
          "516.5 122.6 510.1 117.3 500.5 110.4 485.6 101.8 473.3 96.5 460 92.2 450.9 90.1",
          "440.3 88.5 435.5 88.5 434.9 88 417.3 88 416.8 88.5 408.3 89 397.6 91.2",
          "384.3 95.4 365.6 104.5 356.5 110.4 343.2 121 332.8 131.4 326.4 138.9 313.6 158.1",
          "306.1 173.6 301.3 187.4 298.1 201.8 297.3 210.6 Z")),
        diatom_drawing("829 140 232 263", "diatom-needle", paste(
          "M 830.1 379.1 830.9 388.4 833.1 393.2 837.6 397.8 840.8 399.4 844.5 400.4 852.5 400.4",
          "860 398.8 867.5 396.2 878.1 390.8 887.2 385 908 368.5 936.3 343.4 969.1 311.1",
          "993.6 283.9 1014.4 257.8 1032 232.2 1045.9 208.8 1054.9 189.6 1058.1 178.9 1059.2 173",
          "1059.2 168.2 1059.7 167.7 1059.2 159.2 1058.1 154.9 1054.9 149 1052 146.1 1044 141.8",
          "1036 140.2 1029.1 140.2 1017.9 142.4 1004 147.7 989.6 155.7 968.8 170.1 946.9 188.2",
          "929.1 205.6 905.6 232.2 886.4 257.3 868.8 282.3 853.3 306.9 840.5 332.5 834.7 349",
          "830.9 366.1 830.1 379.1 Z")),
        diatom_drawing("1281 529 213 236", "diatom-triangle", paste(
          "M 1281.9 579.1 1282.1 588.4 1284.8 599.1 1288.5 608.7 1296 623.6 1326.9 673.2 1353.1 723.3",
          "1365.9 744.6 1371.7 752.1 1376.8 757.2 1381.1 760.4 1387.5 763 1398.7 762.5 1406.1 759.3",
          "1409.3 757.2 1415.2 752.4 1422.4 744.1 1427.2 736.6 1433.1 724.9 1453.3 668.4 1474.1 623.1",
          "1484.3 598 1490.1 577.7 1491.7 567.1 1491.2 555.3 1489.6 550 1486.9 545.2 1481.9 540.2",
          "1473.9 535.4 1466.4 532.7 1457.3 530.6 1449.9 530 1449.3 529.5 1426.4 529.5 1425.9 530",
          "1413.1 531.1 1364 541.2 1328.3 545.5 1308.5 549.7 1296.3 555.6 1287.5 563.9 1283.7 570.3",
          "1282.1 575.6 1281.9 579.1 Z")),
        diatom_drawing("644 951 84 86", "diatom-disc", paste(
          "M 645.1 991.3 644.8 1001.7 645.9 1008 647.5 1013.4 652.3 1021.9 657.9 1027.5 662.7 1030.7",
          "670.7 1033.9 679.2 1035.5 687.2 1035.5 695.7 1033.9 703.7 1030.7 710.7 1025.9 715.2 1021.4",
          "719.5 1015.5 723.7 1006.5 725.9 996.3 725.3 985.1 723.2 978.2 717.9 969.7 711.7 963.5",
          "703.7 958.2 696.8 955 686.7 952.3 681.3 952.3 680.8 951.8 674.4 952.3 668.5 953.9",
          "661.1 958.2 655.5 963.8 651.2 970.2 648 977.1 645.9 984.6 645.1 991.3 Z")),
        diatom_drawing("819 988 79 83", "diatom-mini", paste(
          "M 819.5 1029.6 819.7 1041.1 821.9 1048 825.6 1054.4 831.2 1060.6 836.5 1064.3 845.1 1068",
          "849.9 1069.1 860 1069.1 868 1067 873.9 1064.3 880.8 1059.5 886.9 1053.4 892.3 1045.4",
          "894.9 1038.4 896 1033.1 896 1025.6 893.9 1017.6 890.1 1010.7 887.5 1007 879.2 998.7",
          "872.3 993.9 864.3 990.2 857.3 988.6 850.4 988.6 844.5 990.2 840.3 992.3 837.1 994.5",
          "830.9 1000.6 826.1 1007.5 821.9 1017.1 820.3 1023 819.5 1029.6 Z")),
        diatom_drawing("587 216 141 128", "diatom-oval", paste(
          "M 587.5 287.4 587.2 302.1 588.8 310.1 592 318.6 594.7 323.4 601.9 332.2 606.7 335.9",
          "613.1 339.1 621.1 341.3 634.4 341.8 650.4 339.1 668.5 332.7 683.5 324.7 696.3 315.1",
          "705.6 305.8 714.1 294.6 722.1 279.1 725.3 267.4 724.8 253 721.6 244.5 716.3 236.5",
          "707.5 228.2 697.3 222.4 686.7 218.6 677.6 217 669.6 217 669.1 216.5 668 217",
          "660 217 646.7 219.7 634.4 224.5 625.9 229.3 617.9 235.1 608 244.5 597.3 258.9",
          "592 269.5 588.8 279.1 587.5 287.4 Z")),
        diatom_drawing("898 959 237 236", "diatom-round-bottom", paste(
          "M 898.4 1071.8 898.1 1085.4 898.7 1085.9 899.2 1095 901.9 1106.7 904.5 1114.7 907.7 1122.2",
          "913.6 1132.8 926.4 1149.9 940 1162.9 949.1 1169.9 959.2 1176.3 972 1182.7 986.9 1188",
          "998.1 1190.7 1009.9 1191.7 1010.4 1192.3 1030.1 1191.7 1041.9 1189.6 1052 1186.4 1061.1 1182.7",
          "1070.1 1177.9 1079.2 1172 1089.3 1164 1105.1 1147.8 1110.4 1140.8 1118.4 1128 1124.3 1115.8",
          "1128.5 1103.5 1130.7 1094.4 1131.7 1081.6 1132.3 1081.1 1132.3 1069.9 1131.7 1069.4 1131.2 1059.8",
          "1129.6 1051.2 1126.4 1040 1122.1 1029.4 1115.7 1017.6 1110.4 1009.6 1102.4 1000.1 1090.9 989.1",
          "1081.9 982.2 1064.8 972.1 1050.9 966.2 1038.7 962.5 1028 960.3 1018.4 959.8 1017.9 959.3",
          "1004 959.3 1003.5 959.8 994.9 960.3 983.2 963 975.2 965.7 963.5 971 956 975.3",
          "944.8 983.3 936.8 990.2 927.5 1000.1 921.6 1007.5 914.7 1018.2 909.3 1028.3 905.6 1037.4",
          "901.9 1049.1 899.2 1061.9 898.4 1071.8 Z")),
        diatom_drawing("149 283 238 248", "diatom-left-triangle", paste(
          "M 149.6 506 149.3 514.3 149.9 516.4 151.5 520.2 155.5 525.2 158.1 527.4 161.9 529",
          "172 528.4 184.3 522.6 210.9 503.4 219.5 498 232.8 491.6 245.1 487.4 269.6 481",
          "271.7 479.9 297.9 474 308.5 473 309.1 472.4 314.4 472.4 314.9 471.9 349.6 470.8",
          "359.2 469.2 369.9 466 375.7 463.4 382.4 457.8 384.5 453.5 384 446 380.8 439.6",
          "377.6 435.4 367.7 425.5 352.8 413.8 336.8 402.6 322.9 390.8 280 346.9 273.6 339.4",
          "265.1 327.1 253.3 304.2 245.3 292.5 241.3 288.5 234.9 284.7 231.2 283.7 223.7 283.7",
          "217.9 286.3 214.4 289.8 211.2 295.7 207.5 310.1 205.3 326.6 204.3 329.8 202.1 343.7",
          "188.3 399.1 172.8 447.1 163.7 470.6 151.5 498.3 149.6 506 Z")),
        diatom_drawing("371 958 169 197", "diatom-long-small", paste(
          "M 372 970.5 371.7 979.8 373.3 987.8 375.5 994.2 388.8 1020.3 400 1039 409.1 1052.3",
          "423.5 1071 437.9 1087.5 479.2 1128.8 489.9 1137.9 498.9 1144.3 510.7 1150.2 519.7 1152.3",
          "526.1 1152.3 529.9 1151.2 533.1 1149.1 536 1145.6 538.1 1140.8 538.7 1129.6 536 1120",
          "530.7 1107.8 520 1089.1 505.1 1067.2 489.1 1046.4 472 1026.7 445.1 999.3 430.7 985.9",
          "412.5 970.5 399.7 961.9 394.9 959.8 388.5 958.2 381.6 958.7 377.9 960.3 374.9 962.7",
          "372.8 966.5 372 970.5 Z")),
        diatom_drawing("107 549 211 209", "diatom-gold-disc", paste(
          "M 107.5 648.4 107.2 655.6 107.7 656.1 108.3 667.3 111.5 681.7 114.1 689.7 121.6 705.7",
          "126.9 714.2 133.9 723.3 138.9 728.9 148.5 737.4 156 742.8 165.6 748.1 176.8 752.4",
          "183.2 754 193.3 755.6 200.3 755.6 200.8 756.1 213.6 755.6 214.1 755 224.8 754",
          "233.9 751.8 245.1 748.1 262.1 740.1 278.7 728.9 293.9 714.2 300.8 705.2 305.1 698.2",
          "308.8 690.8 312 682.3 314.1 674.3 315.2 663.1 315.7 662.5 315.7 648.7 315.2 648.1",
          "314.7 639.6 312 627.3 309.3 619.3 301.9 603.3 296 594.3 289.1 585.7 281.3 578",
          "270.7 569.5 254.1 559.9 246.7 556.7 233.3 552.4 216.8 549.7 197.1 550.3 186.4 552.4",
          "177.9 555.1 162.9 562 154.4 567.3 144.8 574.8 130.1 590 120 604.9 113.6 618.3",
          "110.9 626.3 108.3 638 107.5 648.4 Z")),
        diatom_drawing("62 468 80 79", "diatom-left-disc", paste(
          "M 62.7 507.6 62.9 515.4 64.5 522.3 68.3 530.3 70.9 534 75.5 538.6 78.7 540.7",
          "82.9 542.8 90.9 544.9 103.2 544.9 110.7 543.3 115.5 541.8 126.1 535.9 132.8 529.8",
          "137.1 523.4 139.2 517.5 139.7 510.6 140.3 510 139.7 503.1 138.1 496.7 134.4 488.7",
          "130.1 482.8 126.1 478.8 119.7 474 110.7 469.8 103.7 468.2 93.6 468.2 89.3 469.2",
          "82.4 472.4 78.7 475.1 71.5 482.3 65.6 492.4 62.9 502 62.7 507.6 Z")),
        diatom_drawing("1142 959 110 130", "diatom-right-needle", paste(
          "M 1143.2 1075.5 1142.9 1079 1144 1082.2 1146.9 1085.1 1149.1 1086.2 1154.4 1086.7 1162.9 1083.5",
          "1176.8 1072.8 1204.3 1044.8 1217.1 1029.4 1227.2 1015.5 1238.4 997.9 1247.5 980.9 1250.1 972.9",
          "1250.1 967.5 1248.5 962.7 1245.6 959.8 1244.5 959.3 1238.7 959.8 1228.5 965.7 1217.3 974.2",
          "1205.1 984.9 1190.4 1000.1 1177.6 1015.5 1161.1 1039 1152.5 1053.4 1146.1 1066.2 1143.2 1075.5 Z")),
        diatom_drawing("898 1216 109 119", "diatom-tiny-triangle", paste(
          "M 898.9 1231.2 898.7 1238.9 900.3 1248.5 909.3 1277.9 913.6 1296 914.1 1300.8 920 1321.6",
          "923.2 1327.4 927.7 1331.4 934.1 1332.5 940 1330.9 946.9 1326.6 954.1 1319.4 973.9 1291.2",
          "995.2 1265.1 1001.1 1256 1004.3 1249.1 1005.3 1245.3 1004.8 1237.3 1002.1 1233.1 998.1 1229.6",
          "991.2 1225.9 983.7 1223.2 968.8 1220.5 944.8 1219.5 944.3 1218.9 940 1218.9 922.9 1216.3",
          "914.9 1216.3 907.5 1218.4 901.9 1222.9 899.7 1226.7 898.9 1231.2 Z")),
        diatom_drawing("1062 537 190 162", "diatom-inner-triangle", paste(
          "M 1062.7 642 1062.4 646.5 1062.9 648.1 1065.9 651.1 1069.6 653.2 1077.1 655.9 1096.8 660.1",
          "1112.8 661.7 1113.3 662.3 1118.7 662.3 1119.2 662.8 1144.3 664.9 1162.9 668.1 1181.1 674",
          "1204.5 686.8 1218.4 693.2 1231.7 696.9 1239.7 696.9 1242.9 695.9 1246.9 692.4 1249.1 688.1",
          "1250.1 682.8 1250.1 676.9 1248 666.3 1238.9 641.7 1236.3 626.3 1236.8 609.7 1237.3 609.2",
          "1239.5 586.3 1240 585.7 1240.5 574.5 1241.1 574 1240.5 555.9 1237.3 546.3 1231.2 540.2",
          "1227.5 538.6 1222.1 537.5 1216.3 537.5 1208.3 539.1 1197.6 543.9 1188.5 549.7 1164.5 568.9",
          "1150.7 578.5 1129.9 590.8 1101.6 605.7 1088.3 614.3 1078.1 622.3 1068.8 631.6 1064.5 637.5",
          "1062.7 642 Z")),
        diatom_drawing("755 1362 93 50", "diatom-tiny-needle", paste(
          "M 755.5 1402.4 755.2 1404.8 756.5 1407.2 759.2 1408.8 763.5 1409.8 768.3 1409.8 779.5 1407.7",
          "797.1 1402.4 816.8 1393.3 836.5 1381.6 843.2 1374.9 845.3 1369.6 844.8 1366.9 841.3 1363.4",
          "834.4 1362.4 820.5 1365 807.7 1369.3 784.3 1380 767.2 1390.1 761.9 1393.8 756.8 1398.9",
          "755.5 1402.4 Z")),
        tags$div(class = "project-intro",
          tags$h1("Global River Chemistry"),
          tags$p("We bring together stream chemistry and watershed data from around the world to study what controls dissolved silicon in rivers and how those patterns change over time."),
          tags$a(class = "project-button", href = project_url, "Project Website"),
          tags$a(class = "project-button", href = "https://github.com/global-river-chem", "Project GitHub")
        ),
        tags$div(class = "project-stats",
          tags$div(tags$strong(format(nrow(all_drivers_table), big.mark = ",")), tags$span("Stream sites")),
          tags$div(tags$strong(nlevels(all_drivers_table$Country)), tags$span("Countries and regions")),
          tags$div(tags$strong(nlevels(all_drivers_table[["Observation Network"]])), tags$span("Observation networks")),
          tags$div(tags$strong(length(summaries$solutes)), tags$span("Solutes"))
        )
      ),
      tags$section(class = "project-section",
        tags$h2("Manuscripts"), tags$ul(class = "paper-list", paper_links)),
      tags$section(class = "project-section",
        tags$h2("Project Team"), tags$div(class = "team-grid", team_cards))
    )
  ),

  # ---- data availability ----
  tabPanel("Data Overview",
    tags$div(class = "workspace-stack",
      bslib::card(full_screen = TRUE, fill = FALSE,
        bslib::card_header("Global River Chemistry Sites"),
        tags$div(class = "map-with-controls",
          tags$aside(`aria-label` = "Map controls",
            selectInput("map_option", "Color sites by",
              choices = driver_choices(map_color_options), selected = "Country"),
            tags$p("Color sites by country, observation network, a watershed characteristic, or a site-average solute concentration."),
            tags$p("Click a marker to see the site and its value for the selected field."),
            tags$p("Wide-ranging fields use a log-like color scale. The values shown are in the original units.")
          ),
          tags$div(class = "map-with-legend",
            leafletOutput("availability_map", height = "500px"),
            tags$aside(class = "map-legend", `aria-label` = "Map legend",
              tags$div(id = "availability_legend", `aria-live` = "polite"))
          )
        )
      ),
      bslib::card(full_screen = TRUE, fill = FALSE,
        bslib::card_header("Solute Availability"),
        plotOutput("plot1", height = "420px"),
        tags$script(HTML("
          new ResizeObserver(function(entries) {
            const width = Math.round(entries[0].contentRect.width);
            if (width && window.Shiny) Shiny.setInputValue('solute_plot_width', width);
          }).observe(document.getElementById('plot1'));
        "))
      ),
      bslib::card(fill = FALSE,
        bslib::card_header("Updated Dataset"),
        tags$p(class = "small", "Highlighted rows indicate data products added since ",
          HTML(paste0(source_link("https://doi.org/10.5066/P138M8AR", "GlASS 2.0"), "."))),
        DT::DTOutput("table1")
      )
    )
  ),

  # ---- site locations ----
  tabPanel("Site Data",
    tags$div(class = "workspace-stack",
      bslib::card(fill = FALSE,
        bslib::card_header("Site Directory"),
        bslib::layout_column_wrap(width = "210px",
          selectInput("site_network", width = "100%", tags$strong("Observation network:"),
            choices = c("All networks" = "", sort(levels(all_drivers_table[["Observation Network"]])))),
          selectInput("site_country", width = "100%", tags$strong("Country:"),
            choices = c("All countries" = "", sort(levels(all_drivers_table$Country))))
        ),
        uiOutput("site_network_summary"),
        bslib::layout_column_wrap(width = "210px",
          tags$div(
            selectizeInput("site_solutes", width = "100%", tags$strong("Has measurements for:"),
              choices = solute_choices[order(names(solute_choices))],
              multiple = TRUE, options = list(placeholder = "Select solutes")),
            tags$p(class = "small", "Sites must have measurements for every selected solute.")
          ),
          selectInput("site_discharge", width = "100%", tags$strong("Has discharge data:"), choices = c("All" = "", "Yes", "No"))
        ),
        selectInput("site_wrtds", tags$strong("Flagged for WRTDS:"), choices = c("All" = "", "Yes", "No")),
        tags$p("Expand Solute Coverage or Source Links for each site."),
        DT::DTOutput("table2")
      )
    )
  ),

  tabPanel("CAMREX Greenup",
    bslib::card(fill = FALSE,
      bslib::card_header("MODIS greenup in the Updated dataset"),
      selectInput("camrex_greenup_site", "Watershed", choices = camrex_sites),
      selectInput("camrex_greenup_year", "Year", choices = 2001:2024, selected = 2002),
      tags$p("Coverage is the share of the watershed with a best- or good-quality date. Coverage below 50% is flagged as low."),
      tableOutput("camrex_greenup_cycles")
    )
  ),

  # ---- data exploration ----
  tabPanel("Data Exploration",
    tags$div(class = "workspace-stack",
      bslib::card(fill = FALSE,
        bslib::card_header("Filter sites"),
        selectizeInput("exploration_filters", tags$strong("Choose up to three numeric variables:"),
          choices = exploration_filter_choices, multiple = TRUE, options = list(maxItems = 3, closeAfterSelect = TRUE)),
        tags$p(class = "small", "The map and plots use sites that meet every selected filter. Sites without a value for a selected filter are left out."),
        uiOutput("exploration_filter_controls"),
        textOutput("exploration_site_count"),
        textOutput("exploration_map_note"),
        selectInput("exploration_map_option", "Color sites by",
          choices = driver_choices(map_color_options), selected = "Country"),
        tags$div(class = "map-with-legend",
          leafletOutput("exploration_map", height = "420px"),
          tags$aside(class = "map-legend", `aria-label` = "Filtered map legend",
            uiOutput("exploration_map_legend")))
      ),
      bslib::navset_card_tab(
        bslib::nav_panel("Distributions",
          tags$h3("Explore Distributions of Solutes and Catchment Characteristics"),
          htmltools::p("Please select a solute or catchment characteristic from the dropdown list below to explore distributions of
                              the selected variable across the dataset. All solute concentrations are in mg L⁻¹."),
          analysis_layout(
            analysis_controls(
              selectInput(inputId = "driver_solute_dropdown",
                label = "Variable",
                choices = driver_choices(quant_drivers), selected = quant_drivers[1])
            ),
            tagList(
              plotOutput("driver_histogram"),
              uiOutput("histogram_years_section")
            )
          )
        ),
        bslib::nav_panel("Relationships",
          tags$h3("Explore Relationships between Solutes and Catchment Characteristics"),
          htmltools::p("Please select a solute or catchment characteristic from each of the dropdown lists below. All solute concentrations are in mg L⁻¹."),
          analysis_layout(
            analysis_controls(
              selectInput(inputId = "x_axis_dropdown",
                label = "X-Axis Variable",
                choices = driver_choices(quant_drivers), selected = "Precipitation (mm yr⁻¹)"),
              selectInput(inputId = "y_axis_dropdown",
                label = "Y-Axis Variable",
                choices = driver_choices(quant_drivers), selected = "DSi"),
              selectInput(inputId = "color_dropdown",
                label = "Color Variable",
                choices = driver_choices(color_drivers), selected = "Dominant Land Cover"),
              actionButton("LR_button", "Show or hide regression")
            ),
            tagList(
              plotOutput("driver_solute_plot"),
              uiOutput("relationship_years_section")
            )
          )
      )
      )
    )
  ),

  # ---- site comparison panel ----
  tabPanel("Site Comparison",
    tags$div(class = "workspace-stack",
      tags$p("Site choices and plots use the filters set in Data Exploration."),
      bslib::navset_card_tab(
        bslib::nav_panel("Solute Distributions",
          tags$h3("Compare Solute Distributions Across Sites"),
          htmltools::p("Using the buttons below, select a solute to compare boxplot distributions of one solute across sites. Select sites using the Country
                              and Site drop down menus. We suggest displaying no more than 10 sites at a time."),
          analysis_layout(
            analysis_controls(
              radioButtons(inputId = "button_solute3",
                label = "Solute",
                choices = solute_choices, selected = "Ca"),
              selectInput(inputId = "dropdown_country3",
                label = "Country",
                choices = chemistry_countries, selected = "United States",
                multiple = TRUE),
              selectInput(inputId = "dropdown_site3",
                label = "Site",
                choices = NULL,
                multiple = TRUE)
            ),
            tagList(
              plotOutput("solute_boxplot")
            )
          )
        ),
        bslib::nav_panel("Time Series",
          tags$h3("Compare Solute Time Series Across Sites"),
          htmltools::p("Using the buttons below, select a solute to compare time series of one solute across sites. Select sites using the Country
                              and Site drop down menus. Switch between individual observations and monthly distributions. We suggest displaying no more than 3 sites at a time."),
          analysis_layout(
            analysis_controls(
              radioButtons(inputId = "button_solute2",
                label = "Solute",
                choices = solute_choices, selected = "Ca"),
              selectInput(inputId = "dropdown_country2",
                label = "Country",
                choices = chemistry_countries, selected = "United States",
                multiple = TRUE),
              selectInput(inputId = "dropdown_site2",
                label = "Site",
                choices = NULL,
                multiple = TRUE),
              actionButton("seasonal_button2", "Switch time-series view")
            ),
            tagList(
              sliderInput("xslider2", label = "Select Date Range of Interest",
                min = chemistry_dates[1], max = chemistry_dates[2], value = chemistry_dates),
              plotOutput("timeseries_out2")
            )
          )
        ),
        bslib::nav_panel("Concentration–Discharge",
          tags$h3("Explore Concentration-Discharge Relationships Across Sites"),
          htmltools::p("Using the buttons below, select a solute to compare concentration-discharge (C-Q) relationships of one solute across sites.
                              Select sites using the Country and Site drop down menus. The equation and R2 in the upper left of the plot
                              provide slope and fit information for each C-Q relationship. We suggest displaying no more than 3 sites at a time."),
          analysis_layout(
            analysis_controls(
              radioButtons(inputId = "button_solute4",
                label = "Solute",
                choices = solute_choices[solute_choices %in% summaries$paired_solutes], selected = "Ca"),
              selectInput(inputId = "dropdown_country4",
                label = "Country",
                choices = paired_countries, selected = "United States",
                multiple = TRUE),
              selectInput(inputId = "dropdown_site4",
                label = "Site",
                choices = NULL,
                multiple = TRUE)
            ), # close sidebar panel
            tagList(
              plotOutput("cq_plot")
            )
          )
      )
      )
    )
  ),

  # ---- time series panel ----
  tabPanel("Site Specific Time Series",

    fluidPage(
      # first set of plots to look at solute time series
      tags$h2("Compare Multi-Solute Time Series at One Site"),
      htmltools::p("Using the buttons below, select multiple solutes to time series solute concentrations at one site. Select one site using the Country
                          and Site menus. Use the toggle to switch between
                          individual measurements and monthly concentration distributions."),

      # side bar layout allows you to add box on the side of the page, good for plotting
      analysis_layout(
        analysis_controls(
          # select input creates a dropdown menu
          # this lists all countries in the master chemistry data
          selectInput(inputId = "dropdown_country",
            label = "Country",
            choices = chemistry_countries, selected = "United States"),

          # this takes the output from the observation network and lists all sites in the selected country
          # see input "dropdown_site" in server below
          selectInput(inputId = "dropdown_site",
            label = "Site",
            choices = NULL),

          # choose one or more solutes for this site
          checkboxGroupInput(inputId = "button_spp",
            label = "Solute",
            choices = solute_choices, selected = "Ca"),

          # switch between the full and seasonal time series
          actionButton("seasonal_button", "Switch time-series view")
        ),

        # show the plot beside its controls
        tagList(
          sliderInput("xslider", label = "Select Date Range of Interest",
            min = chemistry_dates[1], max = chemistry_dates[2], value = chemistry_dates),
          plotOutput("timeseries_out")
        )
      )
    ) # close fluid page
  ), # close panel

  # ---- project history ----
  tabPanel("Project History",
    tags$main(class = "project-page",
      tags$section(class = "project-section",
        tags$h1("Project History"),
        tags$p("Our collaboration began in 2020 with ",
          tags$a(href = "https://lternet.edu/working-groups/from-poles-to-tropics-a-multi-biome-synthesis-investigating-the-controls-on-river-si-exports/",
            "From Poles to Tropics", .noWS = "after"), ", a synthesis working group supported by the Long Term Ecological Research (LTER) Network Office and the National Center for Ecological Analysis and Synthesis (NCEAS) at UC Santa Barbara."),
        tags$p("The group brought together river chemistry and flow records to study how silicon concentrations and exports vary across biomes. That work led to ",
          tags$a(href = "https://doi.org/10.1038/s41597-025-05937-2", "GlASS, the Global Aggregation of Stream Silica", .noWS = "after"),
          ", and continued through a ",
          tags$a(href = "https://www.usgs.gov/centers/john-wesley-powell-center-for-analysis-and-synthesis/news/powell-center-working-groups-2",
            "USGS Powell Center working group"), " investigating changes in riverine silicon.")
      ),
      tags$section(class = "project-section",
        tags$h2("Funding and Support"),
        tags$ul(
          tags$li("Early synthesis: LTER Network Office (NSF awards 1545288 and 1929393) and NCEAS at UC Santa Barbara."),
          tags$li("Follow-on synthesis: USGS John Wesley Powell Center for Analysis and Synthesis (award G25AS00238)."),
          tags$li("Related research: NSF ULTRA-Data awards 2454065, 2454066, and 2454067.")
        ),
        tags$p("See the ",
          tags$a(href = "https://doi.org/10.1029/2025GB008926", "published funding acknowledgments"),
          " for additional details.")
      ),
      tags$section(class = "project-section",
        tags$h2("Workshop History"),
        tags$h3("Synthesize It!"),
        tags$p(tags$strong("Exploring Watershed Function with Stream Data")),
        tags$p(class = "paper-journal", "June 2024"),
        tags$p("The first iteration of this Shiny app was used in this workshop."),
        tags$p("Participants compared stream chemistry and flow with watershed climate, land cover, lithology, and topography to explore how differences among watersheds might help explain differences in stream chemistry."),
        tags$details(class = "workshop-resources",
          tags$summary("Archived workshop resources"),
          tags$ul(
            tags$li(tags$a(href = "https://www.sciencebase.gov/catalog/item/646610b3d34ec11ae4a76b25",
              "Dissolved silicon concentration and yield estimates")),
            tags$li(tags$a(href = "https://www.sciencebase.gov/catalog/item/6511aeabd34e823a0275dc3e",
              "Monthly dissolved silicon concentrations")),
            tags$li(tags$a(href = "https://github.com/sidneyabush/global-river-chem-shiny", "Shiny app source")),
            tags$li(tags$a(href = "https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html",
              "Getting Started with Shiny"))
          ),
          tags$p("Workshop contacts: Keira Johnson (johnkeir@oregonstate.edu) and Sidney A. Bush (bushsi@oregonstate.edu)."),
          tags$p("Project PIs: Kathi Jo Jankowski (kjankowski@usgs.gov), Joanna Carey (jcarey@babson.edu), and Pamela Sullivan (pamela.sullivan@oregonstate.edu).")
        )
      ),
      tags$footer(class = "project-footer",
        tags$h2("Project and Workshop Partners"),
        tags$div(class = "partner-grid",
          partner_logo("National Center for Ecological Analysis and Synthesis", "nceas.png"),
          partner_logo("Long Term Ecological Research Network", crop = "421 618 252 240"),
          partner_logo("National Science Foundation", "nsf.png"),
          partner_logo("U.S. Geological Survey", "usgs.png"),
          partner_logo("John Wesley Powell Center", "powell_center.jpg"),
          partner_logo("Upper Mississippi River Restoration", "umrr.jpg"),
          partner_logo("Oregon State University", "oregon_state_university.webp"),
          partner_logo("Babson College", crop = "73 627 283 189"),
          partner_logo("Critical Zone Network", crop = "43 850 635 230")
        ),
        tags$p(class = "project-license", "This work is licensed under the ",
          tags$a(href = "https://creativecommons.org/licenses/by-nc-sa/4.0/",
            "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License", .noWS = "after"),
          ".")
      )
    )
  )
) # close ui
