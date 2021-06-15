--Nome: Eduardo Radaelli
--Trabalho de Paradigmas de Programação
import Text.Printf

svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgEnd :: String
svgEnd = "</svg>"

svgCircle :: Int -> Int -> Int -> Int -> String -> String -> String
svgCircle a b c d e f = 
  printf "<circle cx='%d' cy='%d' r='%d' stroke-width='%d' stroke='%s' fill='%s' />" a b c d e f

svgLine :: Int -> Int -> Int -> Int -> Int -> String -> String
svgLine a b c d e f =
  printf "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke-width='%d' stroke='%s' />" a b c d e f

svgRect :: Int -> Int -> Int -> Int -> Int -> String -> String -> String
svgRect a b c d e f g = 
  printf "<rect x='%d' y='%d' width='%d' height='%d' stroke-width='%d' stroke='%s' fill='%s' />"a b c d e f g

svgText :: Int -> Int -> Int -> String -> String -> String
svgText a b c d e =
  printf "<text x='%d' y='%d' font-size='%d' fill='%s'> %s </text>" a b c d e

calchorasX :: Int -> Int
calchorasX x = 
  if (x==12) then 1500 else if (x==1) then 1680 else if (x==2) then 1760 else if (x==3) then 1810 
    else if (x==4) then 1800 else if (x==5) then 1700 else if (x==6) then 1500 else if (x==7) then 1280 
    else if (x==8) then 1170 else if (x==9) then 1130 else if (x==10) then 1200 else if (x==11) then 1320 else 0

calchorasY :: Int -> Int
calchorasY x = 
  if (x==12) then 950 else if (x==1) then 1000 else if (x==2) then 1080 else if (x==3) then 1255
    else if (x==4) then 1450 else if (x==5) then 1550 else if (x==6) then 1620 else if (x==7) then 1555
    else if (x==8) then 1405 else if (x==9) then 1255 else if (x==10) then 1080 else if (x==11) then 990 else 0
                          
calcminutosX :: Int -> Int
calcminutosX x = if (x<=3 && x>58) then 1500 else if (x>=4 && x<=8) then 1770 else if (x>=9 && x<=13) then 1980 
  else if (x>=14 && x<=18) then 2050 else if (x>=19 && x<=23) then 1960 else if (x>=24 && x<=28) then 1800 
  else if (x>=29 && x<=33) then 1500 else if (x>=34 && x<=38) then 1230 else if (x>=39 && x<=43) then 1050 
  else if (x>=44 && x<=48) then 950 else if (x>=49 && x<=53) then 1060 else if (x>=54 && x<=58) then 1250 else 0

calcminutosY :: Int -> Int
calcminutosY x = if (x<=3 && x>58) then 700 else if (x>=4 && x<=8) then 800 else if (x>=9 && x<=13) then 950 
  else if (x>=14 && x<=18) then 1250 else if (x>=19 && x<=23) then 1520 else if (x>=24 && x<=28) then 1700 
  else if (x>=29 && x<=33) then 1800 else if (x>=34 && x<=38) then 1675 else if (x>=39 && x<=43) then 1480 
  else if (x>=44 && x<=48) then 1250 else if (x>=49 && x<=53) then 1010 else if (x>=54 && x<=58) then 800 else 0

corFundo :: Int -> String
corFundo x = if (x == 1) then "rgb(255, 0, 0)" else if (x == 2) then "rgb(255, 0, 0)" 
  else if (x == 3) then "rgb(0, 0, 255)" else if (x == 4) then "rgb(0, 255, 0)" 
  else if (x == 5) then "rgb(150, 75, 0)" else if (x == 6) then "rgb(255, 135, 0)"
  else if (x == 7) then "rgb(255, 105, 193)" else if (x == 8) then "rgb(148, 0, 211)"
  else "Erro"

main :: IO ()
main = do 
      putStrLn "Digite a hora: (De 1 a 12)"
      ler1 <- getLine
      let x = (read ler1 :: Int)
      putStrLn "Digite os minutos: (De 0 a 59)" 
      ler2 <- getLine 
      let y = (read ler2 :: Int)
      putStrLn "Digite a cor do fundo da tela: (Digite 1 para vermelho, 2 para amarelo, 3 para azul, 4 para verde, 5 para marrom, 6 para laranja, 7 para rosa, 8 para roxo)"
      ler3 <- getLine
      let cor = (read ler3 ::Int)
      putStrLn "Horas: " 
      print x 
      putStrLn "Minutos: " 
      print y 
      putStrLn "Cor escolhida: "
      print cor
      let svgAll = 
            svgBegin 3000 2500 ++ 
            (svgRect 5 5 2995 2495 10 "rgb(255, 255, 255)" (corFundo cor)) ++
            (svgCircle 1500 1250 900 100 "rgb(170, 170, 170)" "rgb(60, 60, 60)") ++
            (svgLine 0 0 695 695 15 "rgb(0, 0, 0)") ++
            (svgLine 3000 0 2305 695 15 "rgb(0, 0, 0)") ++
            (svgLine 0 2500 695 1805 15 "rgb(0, 0, 0)") ++
            (svgLine 3000 2500 2305 1805 15 "rgb(0, 0, 0)") ++
            (svgText 1400 650 220 "rgb(255, 0, 0)" "12") ++ 
            (svgText 1800 750 220 "rgb(0, 255, 0)" "1") ++ 
            (svgText 2050 980 220 "rgb(0, 0, 255)" "2") ++ 
            (svgText 2150 1320 220 "rgb(255, 255, 0)" "3") ++ 
            (svgText 2050 1650 220 "rgb(0, 255, 255)" "4") ++ 
            (svgText 1810 1900 220 "rgb(255, 0, 255)" "5") ++ 
            (svgText 1450 2000 220 "rgb(255, 0, 0)" "6") ++ 
            (svgText 1090 1900 220 "rgb(0, 255, 0)" "7") ++ 
            (svgText 850 1650 220 "rgb(0, 0, 255)" "8") ++ 
            (svgText 750 1320 220 "rgb(255, 255, 0)" "9") ++ 
            (svgText 800 1000 220 "rgb(0, 255, 255)" "10") ++ 
            (svgText 1050 750 220 "rgb(255, 0, 255)" "11") ++ 
            (svgCircle 1500 1250 50 0 "rgb(255, 255, 255)" "rgb(0, 0, 0)") ++
            (svgLine 1500 1250 (calchorasX x) (calchorasY x) 25 "rgb(0, 135, 255)") ++
            (svgLine 1500 1250 (calcminutosX y) (calcminutosY y) 25 "rgb(255, 85, 0)") ++
            svgEnd
        in  writeFile "imagens.svg" svgAll 
      
