{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Produto where

import Import
import Tool

--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formProduto :: Maybe Produto -> Form Produto
formProduto prod =
  renderDivs $
    Produto
      <$> areq
        textField
        ( FieldSettings
            "Nome: "
            Nothing
            (Just "hs12")
            Nothing
            [("class", "myClass")]
        )
        (fmap produtoNome prod)
      <*> areq textField "Franquia: " (fmap produtoFranquia prod)
      <*> areq textField "Descrição: " (fmap produtoDesc prod)
      <*> areq intField "Número de série: " (fmap produtoSerialNumber prod)
      <*> areq doubleField "Preço: " (fmap produtoPreco prod)

auxProdutoR :: Route App -> Maybe Produto -> Handler Html
auxProdutoR rt produto = do
  (widget, _) <- generateFormPost (formProduto produto)
  defaultLayout $ do
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    toWidgetHead
      [lucius| 

    @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;900&display=swap');

         * {
            font-family: 'Roboto', sans-serif;
         }

         body{
            margin: 0 auto;
         }

         header { 
            display: flex;
            flex-direction: row;
            justify-content: space-between;
            align-items: center;
            background-color: #202020;
            padding: 1rem 0 1rem 1rem;
         }

         header img {
            width: auto;
            height: 100px;
         }

         .direita{
            display: flex;
            flex-direction: row;

            margin-right: 1rem;

            color: #f0f0f0;
            font-size: 2rem;

         }

         .conta{
            display: flex;
            flex-direction: row;
            align-items: center;         

         }

         header a, p{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            color: #f0f0f0
         }

         input{
            color: #000;
            margin: 1rem 0;
            padding: 0.5rem;
            border-radius: 0.5rem;
            background-color: #cfcfcf;
            border: hidden;
            outline: none;
         }

         .aButton{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            padding: 0.5rem;
            color: #f0f0f0;
            border-radius: 0.5rem;
            background-color: #f64668;
            border: hidden;
            outline: none;
         }
    |]
    [whamlet|
         <body>
            <header>
               <div class="esquerda">
                  <a class="home" href=@{AdminR}>
                     <img src="https://i.imgur.com/c6K3Xyj.png" alt="logo">

               <div>
                  <nav class="direita">
                     $maybe email <- sess
                        <div class="conta">
                           <p>
                              Logado como: #{email}
                           <form method=post action=@{SairR}>
                              <input class="aButton" type="submit" value="Sair">
                     $nothing
                        <a class="aButton" href=@{UsuarioR}>
                           Criar Conta 
                  
                        <a class="aButton" href=@{EntrarR}>
                           Entrar
                     
                     <a class="aButton" href=@{ListProdAdmR}>
                        Listar Funkos
                     
                     <a class="aButton" href=@{ProdutoR}>
                        Cadastrar Funkos 

            <main>
               <h2>
                 Mexendo no produto
            
               <form action=@{rt} method=post>
                  ^{widget}
                  <input type="submit" value="Registrar">
      |]

getProdutoR :: Handler Html
getProdutoR = auxProdutoR ProdutoR Nothing

postProdutoR :: Handler Html
postProdutoR = do
  ((resp, _), _) <- runFormPost (formProduto Nothing)
  case resp of
    FormSuccess produto -> do
      runDB $ insert produto
      redirect (ListProdAdmR)
    _ -> redirect HomeR

-- SELECT * from produto where id = pid
getDescR :: ProdutoId -> Handler Html
getDescR pid = do
  produto <- runDB $ get404 pid
  (widget, _) <- generateFormPost formQt
  defaultLayout $ do
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    toWidgetHead
      [lucius|

            @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;900&display=swap');

            * {
               font-family: 'Roboto', sans-serif;
            }

            body{
               margin: 0 auto;
            }

            header { 
               display: flex;
               flex-direction: row;
               justify-content: space-between;
               align-items: center;
               background-color: #202020;
               padding: 1rem 0 1rem 1rem;
            }

            header img {
               width: auto;
               height: 100px;
            }

            .direita{
               display: flex;
               flex-direction: row;

               margin-right: 1rem;

               color: #f0f0f0;
               font-size: 2rem;

            }

            .conta{
               display: flex;
               flex-direction: row;
               align-items: center;         

            }

            header a, p{
               margin: 0 1rem 0 0;
               font-size: 2rem;
               color: #f0f0f0
            }

            input{
               color: #000;
               margin: 1rem 0;
               padding: 0.5rem;
               border-radius: 0.5rem;
               background-color: #cfcfcf;
               border: hidden;
               outline: none;
            }

            .aButton{
               margin: 0 1rem 0 0;
               font-size: 2rem;
               padding: 0.5rem;
               color: #f0f0f0;
               border-radius: 0.5rem;
               background-color: #f64668;
               border: hidden;
               outline: none;
            }
            
          |]
    [whamlet|

         <body>
            <header>
               <div class="esquerda">
                  <a class="home" href=@{HomeR}>
                     <img src="https://i.imgur.com/c6K3Xyj.png" alt="logo">

               <div>
                  <nav class="direita">
                     $maybe email <- sess
                        <div class="conta">
                           <p>
                              Logado como: #{email}
                           <form method=post action=@{SairR}>
                              <input class="aButton" type="submit" value="Sair">
                     $nothing
                        <a class="aButton" href=@{UsuarioR}>
                           Criar Conta 
                  
                        <a class="aButton" href=@{EntrarR}>
                           Entrar
                     
                     <a class="aButton" href=@{ListProdR}>
                        Listar Funkos 

                     <a class="aButton" href=@{ListCompraR}>
                        Minhas Compras 

            <main>
               <h1>
                  COMPRANDO PRODUTO

               <h2>
                  Nome: #{produtoNome produto}
               
               <h2>
                  Franquia: #{produtoFranquia produto}

               <h2>
                  Descrição: #{produtoDesc produto}
               
               <h2>
                  Número de série: #{produtoSerialNumber produto}

               <h2>
                  Preço: #{produtoPreco produto}
               
               <form action=@{ComprarR pid} method=post>
                  ^{widget}
                  <input type="submit" value="Comprar">
      |]

getListProdR :: Handler Html
getListProdR = do
  -- produtos :: [Entity Produto]
  produtos <- runDB $ selectList [] [Desc ProdutoPreco]
  defaultLayout $ do
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    toWidgetHead
      [lucius|


         @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;900&display=swap');

         * {
            font-family: 'Roboto', sans-serif;
         }

         body{
            margin: 0 auto;
         }

         header { 
            display: flex;
            flex-direction: row;
            justify-content: space-between;
            align-items: center;
            background-color: #202020;
            padding: 1rem 0 1rem 1rem;
         }

         header img {
            width: auto;
            height: 100px;
         }

         .direita{
            display: flex;
            flex-direction: row;

            margin-right: 1rem;

            color: #f0f0f0;
            font-size: 2rem;

         }

         .conta{
            display: flex;
            flex-direction: row;
            align-items: center;         

         }

         header a, p{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            color: #f0f0f0
         }

         input{
            color: #000;
            padding: 0.5rem;
            border-radius: 0.5rem;
            background-color: #cfcfcf;
            border: hidden;
            outline: none;
         }

         .aButton{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            padding: 0.5rem;
            color: #f0f0f0;
            border-radius: 0.5rem;
            background-color: #f64668;
            border: hidden;
            outline: none;
         }

         main{
            display: flex;
            flex-direction: column;
            align-items: center;
         }

         table {
            border-collapse: collapse;
            width: 100%;
            border-width: 2px;
         }

         th {
            background-color: #4CAF50;
            color: white;
         }

         th, td {
            padding: 15px;
            text-align: center;
         }

         tr:hover {
            background-color: #f5f5f5;
         }

         #centralizado{
            width: 70%
         }
      |]
    [whamlet|

         <body>
            <header>
               <div class="esquerda">
                  <a class="home" href=@{HomeR}>
                     <img src="https://i.imgur.com/c6K3Xyj.png" alt="logo">

               <div>
                  <nav class="direita">
                     $maybe email <- sess
                        <div class="conta">
                           <p>
                              Logado como: #{email}
                           <form method=post action=@{SairR}>
                              <input class="aButton" type="submit" value="Sair">
                     $nothing
                        <a class="aButton" href=@{UsuarioR}>
                           Criar Conta 
                  
                        <a class="aButton" href=@{EntrarR}>
                           Entrar
                     
                     <a class="aButton" href=@{ListProdR}>
                        Listar Funkos 

                     <a class="aButton" href=@{ListCompraR}>
                        Minhas Compras 

            <main>
               <h2>
                  Funkos disponíveis

               <div id=centralizado>
                  <table>
                     <thead>
                        <tr>
                           <th>
                              Nome

                           <th>
                              Franquia

                           <th>
                              Serial Number

                           <th>
                              Preço

                     <tbody>
                        $forall Entity pid prod <- produtos
                           <tr>
                              <td>
                                 <a href=@{DescR pid}>
                                    #{produtoNome prod}
                              
                              <td>
                                 #{produtoFranquia prod}
                              
                              <td>
                                 #{produtoSerialNumber prod}
                              
                              <td>
                                 #{produtoPreco prod}

      |]

getListProdAdmR :: Handler Html
getListProdAdmR = do
  -- produtos :: [Entity Produto]
  produtos <- runDB $ selectList [] [Desc ProdutoPreco]
  defaultLayout $ do
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    toWidgetHead
      [lucius|


         @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;900&display=swap');

         * {
            font-family: 'Roboto', sans-serif;
         }

         body{
            margin: 0 auto;
         }

         header { 
            display: flex;
            flex-direction: row;
            justify-content: space-between;
            align-items: center;
            background-color: #202020;
            padding: 1rem 0 1rem 1rem;
         }

         header img {
            width: auto;
            height: 100px;
         }

         .direita{
            display: flex;
            flex-direction: row;

            margin-right: 1rem;

            color: #f0f0f0;
            font-size: 2rem;

         }

         .conta{
            display: flex;
            flex-direction: row;
            align-items: center;         

         }

         header a, p{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            color: #f0f0f0
         }

         input{
            color: #000;
            padding: 0.5rem;
            border-radius: 0.5rem;
            background-color: #cfcfcf;
            border: hidden;
            outline: none;
         }

         .aButton{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            padding: 0.5rem;
            color: #f0f0f0;
            border-radius: 0.5rem;
            background-color: #f64668;
            border: hidden;
            outline: none;
         }

         main{
            display: flex;
            flex-direction: column;
            align-items: center;
         }

         table {
            border-collapse: collapse;
            width: 100%;
            border-width: 2px;
         }

         th {
            background-color: #4CAF50;
            color: white;
         }

         th, td {
            padding: 15px;
            text-align: center;
         }

         tr:hover {
            background-color: #f5f5f5;
         }

         #centralizado{
            width: 70%
         }
      |]
    [whamlet|

         <body>
            <header>
               <div class="esquerda">
                  <a class="home" href=@{AdminR}>
                     <img src="https://i.imgur.com/c6K3Xyj.png" alt="logo">

               <div>
                  <nav class="direita">
                     $maybe email <- sess
                        <div class="conta">
                           <p>
                              Logado como: #{email}
                           <form method=post action=@{SairR}>
                              <input class="aButton" type="submit" value="Sair">
                     $nothing
                        <a class="aButton" href=@{UsuarioR}>
                           Criar Conta 
                  
                        <a class="aButton" href=@{EntrarR}>
                           Entrar
                     
                     <a class="aButton" href=@{ListProdAdmR}>
                        Listar Funkos
                     
                     <a class="aButton" href=@{ProdutoR}>
                        Cadastrar Funkos 

                   

            <main>
               <h2>
                  Funkos disponíveis

               <div id=centralizado>
                  <table>
                     <thead>
                        <tr>
                           <th>
                              Nome

                           <th>
                              Franquia

                           <th>
                              Serial Number

                           <th>
                              Preço

                           <th>

                           <th>

                     <tbody>
                        $forall Entity pid prod <- produtos
                           <tr>
                              <td>
                                 #{produtoNome prod}
                              
                              <td>
                                 #{produtoFranquia prod}
                              
                              <td>
                                 #{produtoSerialNumber prod}
                              
                              <td>
                                 #{produtoPreco prod}

                              <td>
                                 <a href=@{UpdProdR pid}>
                                    Editar
                              <td>
                                 <form action=@{DelProdR pid} method=post>
                                    <input type="submit" value="X">
      |]

--   defaultLayout
--     [whamlet|

--
--     |]

getUpdProdR :: ProdutoId -> Handler Html
getUpdProdR pid = do
  antigo <- runDB $ get404 pid
  auxProdutoR (UpdProdR pid) (Just antigo)

-- UPDATE produto WHERE id = pid SET ...
postUpdProdR :: ProdutoId -> Handler Html
postUpdProdR pid = do
  ((resp, _), _) <- runFormPost (formProduto Nothing)
  case resp of
    FormSuccess novo -> do
      runDB $ replace pid novo
      redirect (ListProdAdmR)
    _ -> redirect HomeR

postDelProdR :: ProdutoId -> Handler Html
postDelProdR pid = do
  _ <- runDB $ get404 pid
  runDB $ delete pid
  redirect ListProdR
