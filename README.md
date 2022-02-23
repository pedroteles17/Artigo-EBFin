# Introdução ao Repositório

Esse repositório foi construído com o objetivo de facilitar o acesso às bases de dados e códigos utilizados para produção do artigo. Qualquer contribuição é bem vinda e deve ser encaminhada, preferencialmente, para o email pedroteles17@gmail.com.

# Estrutura do Repositório

O repositório contem 3 estruturas principais. 

## Pasta "Dados"

Como o próprio nome indica, a pasta "Dados" contém os dados que foram utilizados para desenvolvimento do artigo. Dentro dessa pasta temos outras 12 pastas, uma para cada mercado (país) considerado. Dentro de cada uma dessas 12 pastas temos os seguintes arquivos:

* "ativos.csv": retorno diário (2001-01-03 a 2021-10-29) de todos os ativos que fazem ou já fizeram parte do índice de mercado daquele país;
* "comp_maior100k.csv": matriz indicando se determinado ativo fazia ou não parte do índice daquele mercado em um dado mês;
* "indice.csv": retorno diário (2001-01-03 a 2021-10-29) do índice de mercado;
* "rf.csv": retorno diário (2001-01-03 a 2021-10-29) da taxa livre de risco daquele mercado.

## Arquivo .R "Funcoes.R"

Esse é o arquivo que será chamado pelo nosso arquivo principal ("Testes.R"). Nele temos todas as funções necessárias para realização dos _backtests_. Por ser um código bastante flexível - permite definir diferentes períodos de estimação, _holding periods_, método para definição dos pesos, etc. -, ele pode ser considerado um código mais complexo. É intenção do autor elaborar, posteriormente, um tutorial apenas sobre a construção desses _backtests_.

## Arquivo .R "Testes.R"

Aqui que efetivamente testamos as hipóteses levantadas. Esse código pode ser dividido em 3 partes:

1. Construção do portfólio de _momentum_ clássico e determinação do drift e retorno acumulado de todos os ativos daquele mercado;
2. Utilização dos dados acima para construção da estatística de diferença de médias. Além disso, realizamos testes F, teste de Granger e teste de Vuong;
3. Após termos os resultados, precisamos transformá-los em tabelas para poderem serem utilizadas no artigo. Assim, nessa terceira e última parte, fazemos exatamente isso.

# Observação

Como temos arquivos com mais de 100MB, foi necessário utilizar o Git Large File Storage.
