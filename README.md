# GLM-EleicoesBH2020
 AJUSTE DE UM MODELO LINEAR GENERALIZADO ELEIÇÕES 2020 - BELO HORIZONTE-MG

Os modelos lineares generalizados são uma classe de modelos que aumentam as
possibilidades de análises para outras distribuições além da distribuição normal. Os MLGs são
utilizados quando o objetivo do estudo é fazer uma análise sobre uma variável resposta do tipo
contagem, categórica ou Assimétrica (com valores positivos) e para ser um MLG é necessário
que a distribuição de probabilidade seja da família exponencial.

Trabalhamos com uma base de dados composta por 1103 observações referentes ao
resultado das eleições na cidade de Belo Horizonte - MG. Para este trabalho foi necessário
juntar duas bases obtidas nos sites do TRE e TSE e em seguida foi realizada uma filtragem neste
banco com objetivo de selecionar apenas os candidatos a vereador que concorreram nas eleições
2020 no município de Belo Horizonte.
Os dados podem ser obtidos em: https://www.tre-mg.jus.br/eleicoes/eleicoes2020/eleicoes-2020 (clicar em ‘Resultados via TRE’) e https://www.tse.jus.br/eleicoes/
estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais (candidatos > 2020 > Candidatos (formato ZIP) )

Sobre as variáveis presentes no banco de dados:
> Candidato: Nome do candidato que aparece na urna.
 Idade: Idade do candidato na data da posse. A idade é calculada com base na data da posse
do referido candidato para o cargo e unidade eleitoral constantes no arquivo de vagas.
 Gênero: Gênero do candidato.
 Escolaridade: Grau de instrução do candidato.
 EstadoCivil: Estado civil do candidato.
 Cor_raça: Cor/raça do candidato. (auto declaração)
 possuiBens: Indica se o candidato tem ou não bens a declarar. Pode assumir os valores: S -
Sim e N - Não. Esta informação é fornecida pelo próprio candidato no momento do pedido
da candidatura.
 Votos: Total de votos obtidos pelo candidato.