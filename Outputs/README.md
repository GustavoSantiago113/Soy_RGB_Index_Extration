# TCC
Extração de índices vegetativos Rn, Gn, Bn, MPRI, H, S, V e ICVE em dossel de soja utilizando uma câmera digital que opera no espectro do visível.
As fotografias foram tiradas em campo a cerca de 1m e 1,6m do solo utilizando uma câmera de alta resolução em um tripé. Havia também um objeto branco no campo de captura
da imagem para que o balanço de branco fosse feito automaticamente.
As imagens tiradas foram armazenadas dentro de uma pasta e o programa possui um loop para realizar a análise de uma de cada vez automaticamente.
O fundo da imagem (solo, palhada e algumas daninhas) é removido através de limiar utilizando diferença entre verde e azul, e após isso, é feito a extração dos índices.
A saída do programa é um arquivo csv onde as colunas são os índices e as linhas as imagens analisadas. O resultado exibido nas linhas e a média do índice para toda a
imagem.
