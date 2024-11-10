-- Author: Eduardo De Bastiani, Erik Suris, João Pedro Kriger, Mariah Freire

import System.IO (hFlush, stdout)

-- Definindo o tipo Item com nome e preço
data Item = Item { nome :: String, preco :: Int } deriving Show

-- Função principal
main :: IO ()
main = do
    putStrLn "Bem-vindo ao Kit de Sobrevivência para Missão Espacial!"
    menu []

-- Função de menu
menu :: [Item] -> IO ()
menu kit = do
    putStrLn "\nMenu Principal:"
    putStrLn "1. Adicionar item ao kit"
    putStrLn "2. Listar todos os itens no kit"
    putStrLn "3. Remover item do kit"
    putStrLn "4. Calcular custo total do kit"
    putStrLn "5. Filtrar itens por intervalo de preço"
    putStrLn "6. Buscar item por nome"
    putStrLn "7. Sair da aplicação"
    putStr "Escolha uma opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> adicionarItem kit
        "2" -> listarItens kit
        "3" -> removerItem kit
        "4" -> calcularCustoTotal kit
        "5" -> filtrarItens kit
        "6" -> buscarItem kit
        "7" -> putStrLn "Saindo da aplicação..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            menu kit


-- Função para adicionar um item ao kit
adicionarItem :: [Item] -> IO ()
adicionarItem kit = do
    putStr "Nome do item: "
    hFlush stdout
    nome <- getLine
    putStr "Preço do item: "
    hFlush stdout
    precoStr <- getLine
    let preco = read precoStr :: Int
    let novoKit = kit ++ [Item nome preco]
    putStrLn "Item adicionado com sucesso!"
    menu novoKit


-- Função para listar todos os itens do kit
listarItens :: [Item] -> IO ()
listarItens [] = putStrLn "O kit está vazio."
listarItens kit = do
    putStrLn "\nItens no kit:"
    mapM_ (\(i, item) -> putStrLn $ show i ++ ". " ++ nome item ++ " - " ++ show (preco item) ++ " créditos") (zip [1..] kit)
    menu kit


-- Função para remover um item do kit
removerItem :: [Item] -> IO ()
removerItem [] = do
    putStrLn "O kit está vazio. Não há itens para remover."
    menu []
removerItem kit = do
    putStrLn "\nItens no kit:"
    mapM_ (\(i, item) -> putStrLn $ show i ++ ". " ++ nome item ++ " - " ++ show (preco item) ++ " créditos") (zip [1..] kit)
    putStr "Digite o número do item para remover: "
    hFlush stdout
    indiceStr <- getLine
    let indice = read indiceStr :: Int
    if indice > 0 && indice <= length kit
        then do
            let novoKit = take (indice - 1) kit ++ drop indice kit
            putStrLn "Item removido com sucesso!"
            menu novoKit
        else do
            putStrLn "Índice inválido. Tente novamente."
            removerItem kit


-- Função para calcular o custo total do kit
calcularCustoTotal :: [Item] -> IO ()
calcularCustoTotal kit = do
    let total = sum (map preco kit)
    putStrLn $ "Custo total do kit: " ++ show total ++ " créditos"
    menu kit


-- Função para filtrar itens por intervalo de preço
filtrarItens :: [Item] -> IO ()
filtrarItens kit = do
    putStr "Preço mínimo: "
    hFlush stdout
    minStr <- getLine
    putStr "Preço máximo: "
    hFlush stdout
    maxStr <- getLine
    let minPreco = read minStr :: Int
    let maxPreco = read maxStr :: Int
    let itensFiltrados = filter (\item -> preco item >= minPreco && preco item <= maxPreco) kit
    if null itensFiltrados
        then putStrLn "Nenhum item encontrado nesse intervalo de preço."
        else do
            putStrLn "Itens no intervalo de preço:"
            mapM_ (\(i, item) -> putStrLn $ show i ++ ". " ++ nome item ++ " - " ++ show (preco item) ++ " créditos") (zip [1..] itensFiltrados)
    menu kit

--buscar item por nome
buscarItem :: [Item] -> IO ()
buscarItem kit = do
    putStr "Digite o nome do item para buscar: "
    hFlush stdout
    nome <- getLine
    encontrarItemPorNome kit nome
    menu kit 

-- Função para encontrar um item pelo nome
encontrarItemPorNome :: [Item] -> String -> IO ()
encontrarItemPorNome [] nomeBuscado = putStrLn "Item não encontrado."
encontrarItemPorNome (item:kit) nomeBuscado
    | nomeBuscado == nome item = putStrLn $ "Item encontrado: " ++ nome item ++ " - " ++ show (preco item) ++ " créditos"
    | otherwise               = encontrarItemPorNome kit nomeBuscado

