{-# LANGUAGE BlockArguments #-}




principalMenu :: [Char]
principalMenu = "Cuestionarios \n" ++ 
                "1-Crear encuestas\n" ++
                "2-Responder encuesta \n" ++
                "3-Ver estadísticas \n" ++
                "4-Salir \n\n" ++
                "Seleccione una opción: "


responseMenu :: [Char]
responseMenu = "1-Responder manualmente\n" ++
               "2-Responder automáticamente\n"++
               "Seleccione una opción: "

questionMenu :: [Char]
questionMenu = "¿Qué tipo de preguntas desea crearle a este cuestionario?\n" ++
                "1- Por escala \n" ++
                "2- Selección única\n" ++
                "Seleccione una opción: \n"


statisticsMenu :: [Char]
statisticsMenu = "¿Qué tipo estadística desea generar?\n" ++
                 "1-Cuántas veces se ha respondido un formulario en específico\n"++
                 "2-Cantidad total de preguntas respondidas\n" ++
                 "3-\n" ++
                 "Seleccione una opción: \n"





data Formulario = Formulario { formName     :: String, questions   :: [Question] }                          deriving (Show, Eq)
data Question   = Question   { questionName :: String, options     :: [String]  }                           deriving (Show, Eq)
data Responses  = Response   { form_name    :: String, question    :: [String],  response ::  [String] }    deriving (Show, Eq)





prueba :: Formulario
prueba = Formulario {formName = "Pais", questions = [Question {questionName = "Cree en el COVID-19?", options = ["b)Si", "a)No"]},Question {questionName = "Le importa que suban los impuestos en el pais?", options = ["c)Tal vez","b)No","a)Si"]},Question {questionName = "Esta de acuerdo con el aborto en el pais?", options = ["b)No","a)Si"]}]}

form_covid :: Formulario
form_covid = Formulario {formName = "COVID", questions = [Question {questionName = "Como ha sido su actividad fisica en tiempo de pandemia? ", options = ["5=Excelente","4=Buena","3=Regular","2=Escasa","1=Nula"]}, Question {questionName = "Como ha llevado las relaciones con sus amigos en tiempos de pandemia?", options = ["5=Muy bien","4=Bien","3=Regular","2=Mal","1=Muy mal"]},Question {questionName = "Que tambien se ha alimentado desde que empezo la pandemia?", options = ["5=Muy bien","4=Bien","3=Regular","2=Mas o menos","1=Mal"]}]}

form_deportes :: Formulario
form_deportes = Formulario {formName = "Deportes", questions = [Question {questionName = "Cual es el deporte conocido como el deporte blanco? ", options = ["d)Volleyball","c)Tennis","b)Natacion","a)Atletismo"]},Question {questionName = "En que deporte juegan once jugadores contra once jugadores?", options = ["c)Volleyball","b)Futbol","a)Basketball"]},Question {questionName = "Cual de los siguientes deportes utiliza una raqueta?", options = ["d)Todas las anteriores","c)Ping Pong","b)Badminton","a)Tennis"]}]}

form_reciclaje :: Formulario
form_reciclaje = Formulario {formName = "Contaminacion", questions = [Question {questionName = "Ha recibido alguna charla sobre el manejo del reciclaje?", options = ["5=Mas de cinco veces","4=Mas de dos veces","3=Dos veces","2=Una vez","1=Nunca"]},Question {questionName = "Como considera el reciclaje en su hogar?", options = ["5=Muy bueno","4=Bueno","3=Regular","2=Malo","1=Muy malo"]},Question {questionName = "Como considera el manejo de los desechos en el TEC", options = ["5=Excelente","4=Muy bueno","3=Bueno","2=Regular","1=Malo"]}]}

formularios :: [Formulario]
formularios = [prueba, form_covid, form_deportes, form_reciclaje]

preguntas :: [Question]
preguntas = []

respuestas :: [Responses]
respuestas = []



crearEncuesta :: [Formulario] -> [Question] -> [Responses] -> IO ()
crearEncuesta lista_encuestas preguntas_global respuestas = do 
                                   
                                   putStrLn "Digite el nombre de la encuesta: "
                                   formName <- getLine

                                   putStrLn questionMenu -- Enseño el menú preguntas
                                   option <- getLine 
                                   
                                   if option == "1"
                                       then 
                                           do
                                               questions_total <- generar_pregunta_x_indice preguntas_global
                                               
                                               let form = Formulario { formName = formName, questions = questions_total }
                                               let lista_global = pegar_listas_formularios form lista_encuestas
                                               let preguntas_lista = []

                                               menu_principal lista_global preguntas_lista respuestas


                                        else 
                                            do
                                            seleccion_total <- generar_preguntas_seleccion preguntas_global
                                            let form = Formulario { formName = formName, questions = seleccion_total }
                                            let lista_global = pegar_listas_formularios form lista_encuestas
                                            let preguntas_reset = []

                                            menu_principal lista_global preguntas_reset respuestas
                                            



generar_preguntas_seleccion :: [Question] -> IO [Question]
generar_preguntas_seleccion preguntas_lista = do
                                                putStrLn "Para las respuestas de selección escribir, ejemplo: a)Si, b)No"
                                                putStrLn "Digite la pregunta que desea generar: "
                                                pregunta <- getLine
                                                
                                                putStrLn "Digite la cantidad de respuestas a generarle a esta pregunta: "
                                                cant_respuestas <- getLine

                                                let cant = ( read cant_respuestas :: Int )

                                                let preguntas_string = []

                                                lista_question <- generar_seleccion 1 cant preguntas_string
                                                
                                                let pregunta_terminada = Question { questionName = pregunta, options = lista_question }

                                                let preguntas_global = pegar_listas_preguntas pregunta_terminada preguntas_lista

                                                putStrLn "Desea agregar otra pregunta? S - Si, N - No"
                                                option <- getLine
                                                
                                                if option  == "S"
                                                    then
                                                        do
                                                            generar_preguntas_seleccion preguntas_global
                                                    else
                                                        return (preguntas_global)
                                

formularios_respondidos :: [Responses] -> String -> [Responses]
formularios_respondidos  respuestas nombre_encuestas = do
                                                        (filter ( \x -> nombre_encuestas == (form_name x) ) respuestas)


imprimir_lista_recursivo :: [Formulario] -> [Question] -> [Responses] -> [Formulario] -> IO ()
imprimir_lista_recursivo lista preguntas respuestas lista_recursiva = do 
                                                    if null (tail lista_recursiva)
                                                        then
                                                            do
                                                                print (formName (head (lista_recursiva)))
                                                                putStrLn "Se debe de respetar espacios y mayúsculas"
                                                                putStrLn "Digite la encuesta que desea consultar: "

                                                                encuesta <- getLine
                                                                let respuesta = length (formularios_respondidos respuestas encuesta)

                                                                if respuesta == 0
                                                                    then
                                                                        do
                                                                            putStrLn "Este formulario no se ha respondido ni una sola vez"
                                                                            menu_principal lista preguntas respuestas
                                                                    else
                                                                        do
                                                                            putStr "Las veces que se ha respondido el formulario es: "
                                                                            print respuesta

                                                                            menu_principal lista preguntas respuestas


                                                                        


                                                        else
                                                            do
                                                                print (formName (head (lista_recursiva)))
                                                                imprimir_lista_recursivo lista preguntas respuestas (tail lista_recursiva)



recorrer_preguntas :: [Responses] -> [Integer]
recorrer_preguntas respuestas  = do
                                   (map (\ x -> toInteger (length (response x))) respuestas)



obtener_total :: (Show t1, Num t1) => t2 -> t3 -> t4 -> [t1] -> t1 -> IO ()
obtener_total lista preguntas respuestas recursivo cantidad_total = do
                                            if null (tail recursivo)
                                                then
                                                    do
                                                        let cantidad_final = head recursivo + cantidad_total
                                                        putStrLn "La cantidad total de preguntas respondidas hasta el momento es de: "
                                                        print cantidad_final
                                                        
                                                else
                                                    do
                                                        let aux  =  head recursivo + cantidad_total
                                                        obtener_total lista preguntas respuestas (tail recursivo) aux
                                                        

                    


opciones_estadisticas :: [Formulario] -> [Question] -> [Responses] -> IO ()
opciones_estadisticas lista preguntas respuestas = do
                                        putStrLn statisticsMenu
                                        option <- getLine
                                        
                                        if option == "1"
                                            then
                                                do
                                                    let recursion = lista
                                                    imprimir_lista_recursivo lista preguntas respuestas recursion
                                                    
                                            else
                                                if option ==  "2"
                                                    then 
                                                        do
                                                            let recursivo = recorrer_preguntas respuestas
                                                           
                                                            obtener_total lista preguntas respuestas recursivo 0 
                                                            menu_principal lista preguntas respuestas
                                                    else
                                                        

                                                        putStrLn "Ultima funcion"



generar_pregunta_x_indice :: [Question] -> IO [Question]
generar_pregunta_x_indice preguntas_array  = do
                                               putStrLn "Debe de generarlas anteponiendo el numero del indice, por ejemplo: 1='Escala'..."
                                               putStrLn "Digite la pregunta que desea generar: "
                                               pregunta <- getLine

                                               let preguntas_string = []
                                               
                                               lista_question <- generar_indices 1 preguntas_string

                                               let pregunta_terminada = Question { questionName = pregunta, options = lista_question }

                                               let preguntas_global = pegar_listas_preguntas pregunta_terminada preguntas_array

                                               putStrLn "¿Desea agregar otra pregunta? S-Si N-No"
                                               option <- getLine

                                               if option == "S"
                                                   then 
                                                       do
                                                           generar_pregunta_x_indice preguntas_global 
                                                    else
                                                        return (preguntas_global)





generar_seleccion :: (Ord t, Num t) => t -> t -> [String] -> IO [String]
generar_seleccion indice cant_respuestas lista_question = do
                                            if indice > cant_respuestas
                                                then
                                                    do
                                                        return lista_question
                                                else
                                                    do
                                                        putStrLn "Digite la respuesta "
                                                        respuesta <- getLine 
                                                        let lista_respuesta = pegar_listas_preguntas respuesta lista_question
                                                        generar_seleccion (indice + 1 ) cant_respuestas lista_respuesta


                      



generar_indices :: (Eq t, Num t) => t -> [String] -> IO [String]
generar_indices indice lista_question = do
    if indice == 6
        then
            do
              return ( lista_question )
        else
            do
                putStrLn "Digite la escala: " 
                nombre_escala <- getLine
                let lista_con_pregunta = pegar_listas_preguntas nombre_escala lista_question
                generar_indices (indice + 1 ) lista_con_pregunta



responderAutomatico :: [Formulario] -> [Question] -> [String] -> [String] -> [Responses] -> [Char] -> IO ()
responderAutomatico lista_global lista_responder preguntas_respondidas respuestas_dadas respuestas_global nombre_formulario = 
    do
        if null (tail lista_responder)
            then 
                do
                    let nombre_pregunta = questionName (head lista_responder)
                    let aux_pregunta_respondida = pegar_listas_preguntas nombre_pregunta preguntas_respondidas 
                    if length (options (head lista_responder)) == 1
                        then 
                            do
                                let respuesta_dada =  reverse (options (head lista_responder)) !! 0
                                let aux_respuesta_dada = pegar_listas_preguntas respuesta_dada respuestas_dadas
                                let respuesta_final = Response { form_name = nombre_formulario, question = aux_pregunta_respondida, response = aux_respuesta_dada }
                                let final_pegada =  pegar_listas_formularios respuesta_final respuestas_global

                                print final_pegada

                                putStrLn "Desea volver a responder este formulario automáticamente? S-Si, N-No"
                                option <- getLine

                                if option == "S"
                                    then 
                                        do
                                            let recursion = lista_global
                                            obtenerFormulario_automatico lista_global final_pegada recursion nombre_formulario
                                    else
                                        do
                                         menu_principal lista_global [] final_pegada
                                

                        else
                            do 
                                let lista_opciones = options (head lista_responder)
                                let respuesta_dada = reverse lista_opciones !! (0 + 1)
                                let aux_respuesta_dada = pegar_listas_preguntas respuesta_dada respuestas_dadas
                                
                        
                                let respuesta_final = Response { form_name = nombre_formulario, question = aux_pregunta_respondida, response = aux_respuesta_dada }
                                let final_pegada =  pegar_listas_formularios respuesta_final respuestas_global

                                print final_pegada
                                
                                putStrLn "Desea volver a responder este formulario automáticamente? S-Si, N-No"
                                option <- getLine

                                if option == "S"
                                    then 
                                        do
                                            let recursion = lista_global
                                            obtenerFormulario_automatico lista_global final_pegada recursion nombre_formulario
                                    else
                                        do
                                            menu_principal lista_global [] final_pegada
                
            else
                do
                    let nombre_pregunta = questionName (head lista_responder)
                    let aux_pregunta_respondida = pegar_listas_preguntas nombre_pregunta preguntas_respondidas
                    let respuesta_dada = options (head lista_responder) !! 0
                    let aux_respuesta_dada = pegar_listas_preguntas respuesta_dada respuestas_dadas
                    responderAutomatico lista_global (tail lista_responder) aux_pregunta_respondida aux_respuesta_dada respuestas_global nombre_formulario




imprimirFormularios :: [Formulario] -> [Formulario] -> [Responses] -> IO ()
imprimirFormularios lista_global recursion_lista respuestas = do
                                    if (null (tail recursion_lista))
                                        then
                                            do
                                                print (formName (head recursion_lista))
                                                putStrLn "Digite el nombre de la encuesta que desea responder"
                                                encuesta_responder <- getLine
                                                let cant = 1
                                                putStrLn responseMenu
                                                option <- getLine
                                                
                                                if option == "1"
                                                    then
                                                        do
                                                            let lista_recursion = lista_global
                                                            obtenerFomulario lista_global lista_recursion encuesta_responder cant respuestas
                                                            putStrLn "Responder manualmente"
                                                        
                                                    else 
                                                        do
                                                            let recursion = lista_global;
                                                            obtenerFormulario_automatico lista_global respuestas recursion encuesta_responder
                                                            putStrLn "Responder automáticamente"
                                        else
                                            do
                                                print (formName (head recursion_lista))
                                                imprimirFormularios lista_global (tail recursion_lista) respuestas





obtenerFomulario :: [Formulario] -> [Formulario] -> String -> t -> [Responses] -> IO ()
obtenerFomulario lista_encuesta lista_recursion nombre_formulario cantidad respuestas = do
                                                                        if nombre_formulario == formName (head lista_recursion)
                                                                            then
                                                                                do 
                                                                                     
                                                                                    let preguntas_lista = reverse (questions (head lista_recursion))
                                                                                    responderEncuesta lista_encuesta respuestas nombre_formulario preguntas_lista [] []
                                                                                                                      
                                                                            else
                                                                                do
                                                                                    obtenerFomulario lista_encuesta (tail lista_recursion) nombre_formulario cantidad respuestas





responderEncuesta :: [Formulario] -> [Responses] -> [Char] -> [Question] -> [String] -> [String] -> IO ()
responderEncuesta lista_encuesta lista_respuestas nombre_formulario preguntas_lista lista_preguntas_respondida lista_respuesta_respondida  = 
    do

                if  null ( tail preguntas_lista )
                    then 
                        do
                            let nombre_pregunta = questionName (head preguntas_lista)
                            putStrLn nombre_pregunta
                            print (reverse(options (head preguntas_lista)))
                            respuesta <- getLine

                            let final_preguntas_respondidas = (pegar_listas_preguntas nombre_pregunta lista_preguntas_respondida)
                            let final_respuestas =  ( pegar_listas_preguntas respuesta lista_respuesta_respondida )

                            let respuesta = Response { form_name = nombre_formulario, question = final_preguntas_respondidas, response = final_respuestas }

                            let final_pegada = pegar_listas_preguntas respuesta lista_respuestas

                            putStrLn "Desea responder otra vez este formulario? S-Si, N-No "
                            option <- getLine

                            if option == "S"
                                then
                                    do
                                        let recursion = lista_encuesta;
                                        obtenerFomulario lista_encuesta recursion nombre_formulario 0 final_pegada
                                else
                                    do 
                                    menu_principal lista_encuesta [] final_pegada

                    else 
                        do
                            let nombre_pregunta = questionName (head preguntas_lista)
                            putStrLn nombre_pregunta
                            print (reverse(options (head preguntas_lista)))
                            respuesta <- getLine
                            let final_preguntas_respondidas =  (pegar_listas_preguntas nombre_pregunta lista_preguntas_respondida)
                            let final_respuestas =  ( pegar_listas_preguntas respuesta lista_respuesta_respondida )
                            responderEncuesta lista_encuesta lista_respuestas nombre_formulario  (tail preguntas_lista) final_preguntas_respondidas final_respuestas




obtenerFormulario_automatico :: [Formulario] -> [Responses] -> [Formulario] -> String -> IO ()
obtenerFormulario_automatico lista_original respuestas recursion nombre_formulario = do
                                                                                        if nombre_formulario == formName (head recursion)
                                                                                            then 
                                                                                                do

                                                                                                    let preguntas_responder = reverse ( questions (head recursion) )

                                                                                                    responderAutomatico lista_original preguntas_responder [] [] respuestas nombre_formulario

                                                                                            else
                                                                                                do
                                                                                                    obtenerFormulario_automatico lista_original respuestas (tail recursion) nombre_formulario
                                                                                                 


                                                                                            
                                                                                                                                                                            

pegar_listas_formularios :: a -> [a] -> [a]
pegar_listas_formularios l b = l : b

pegar_listas_preguntas ::  a -> [a] -> [a]
pegar_listas_preguntas l b = l : b
    



main :: IO ()
main = do menu_principal formularios preguntas respuestas




menu_principal :: [Formulario] -> [Question] -> [Responses] -> IO ()
menu_principal lista preguntas respuestas = do
        putStrLn principalMenu 
        option <- getLine
        if option == "1"
            then
                do
                    crearEncuesta lista preguntas respuestas
            else
                if option == "2"
                    then
                        do  

                            let lista_global = lista
                            imprimirFormularios lista_global lista respuestas 
                            print respuestas
                       
                    
                    else
                        if option == "3"
                            then 
                                do
                                   opciones_estadisticas lista preguntas respuestas
                            else
                                do
                                    print lista
                                    --print respuestas
                                    putStrLn "Saliendo"