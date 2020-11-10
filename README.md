# haskell-site

A site using yesodweb framework

app/Main.hs: Programa principal. Nao será mexido.
src/Foundation.hs: Configuraçoes.
src/Application.hs: Os imports.

routes.yesoroutes
NOME_DA_ROTA TIPO_ASSOCIADO METODO_HTTP
/ HomeR GET ===> getHomeR (DEVEMOS PROGRAMAR UMA FUNÇAO COM ESTE NOME)
/ola OlaR GET ===> getOlaR

src/Home.hs ===> FONTE DO SITE

SEMPRE USE stack build PARA COMPILAR!

# Instructions

1- INSTALAR A FERRAMENTA stack https://docs.haskellstack.org/en/stable/README/

wget -qO- https://get.haskellstack.org/ | sh

OU PELO INSTALADOR

CHECAR: stack --version

2- CRIAR PASTA DE PROJETO: mkdir projeto

cd projeto

3- IR EM: https://github.com/commercialhaskell/stack-templates/wiki

E ESCOLHER O TEMPLATE.

NA AULA DE HOJE, yesodweb/minimal

NAS PROXIMAS, yesodweb/postgres

4- stack new aula yesodweb/minimal

5- ENTRE NA PASTA: cd aula

6- stack install

7- stack build

8- P/ EXECUTAR: stack exec aula
