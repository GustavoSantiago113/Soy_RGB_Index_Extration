# -*- coding: utf-8 -*-
"""
Created on Wed Aug 21 10:02:49 2019

@author: lucas
"""
import cv2
import numpy as np
import glob
import pandas as pd

"""glob identifica todos os arquivos com a mesma extensão dentro da uma pasta nomes dos arquivos devem seguir
uma ordem número de três digitos: 000, 001, 002...010, 011...100, 101..."""
local = glob.glob("C:/Users/lucas/Documents/Python Scripts/Leaf area index/Imagens_Carol/imagens_carol_verso/*.jpg") #OS

lista_imagens = [] #cria uma lista onde serão armazenadas todas as imagens dentro da pasta de trabalho
indices=[] #cria uma lista para armazenar os resultados dos índices calculados

"""loop for img in local, lê todas as imagens encontradas na variável path
cada uma das imagens em local será lida e armazenada no vetor lista_imagens"""
for img in local:
	n = cv2.imread(img)
	lista_imagens.append(n)

img_number = 1 #númerador para identificar a imagem ao exibir

"""loop principal, lê cada uma das imagens no vetor list_image"""
for k in range(len(lista_imagens)):
	"""identificando a imagem no vetor de imagens (list_image) e convertendo em uint8"""
	img_folha = np.uint8(lista_imagens[k])
	
	"""alterando a escala da imagem usando cv2.resize
	aplicando o resize, img é a imagem original, tamanho apresenta as dimensões da nova imagem, 
	interpolation é o tipo deinterpolação utilizada no resize."""
	#print('Original Dimensions : ',img_folha.shape) #mostra o tamanho da imagem original
	porcent_reducao = 20 #porcentagem para qual a imagem original será reduzida (% da imagem original)
	largura = int(img_folha.shape[1] * porcent_reducao / 100) #redução da largura 
	altura = int(img_folha.shape[0] * porcent_reducao / 100) #redução da altura
	tamanho = (largura, altura)	#atribui o tamanho da imagem á uma variável de dimensões
		
	img_redim = cv2.resize(img_folha, tamanho, interpolation = cv2.INTER_AREA)  
	#print('Resized Dimensions antes : ',img_resized.shape)
	img_redim = img_redim[:, 100:700]
	#print('Resized Dimensions depois : ',img_resized.shape) #dimensões da imagem após alterar a escala
		
	linhas, colunas, _ = img_redim.shape #atribui as dimensões da imagem redimensionada à variavel tuple (linhas, colunas)
	img_redim = cv2.GaussianBlur(img_redim,(3,3),cv2.BORDER_WRAP) #suavisa a imagem para evitar contornos falsos
			
	img2=np.zeros((linhas,colunas))#cria uma imagem vazia do tamanho da imagem redimensionada
	
	"""loop for, lê a imagem redimensionada linha por linha, coluna por coluna
	BG5 calcula o valor da divisão ponto a ponto entre verde e azul
	a folha apresenta maior indice de verda da imagem e o menor índice de azul
	ao fazer a divisão, os maiores valores de G/B irão aparecer onde está localizada a folha.
	o limiar 1.2 é então aplicado para segmentar a imagem."""
	for k in range(linhas):
		for n in range(colunas):
			Green_Blue_division = (img_redim[k,n,1])/(img_redim[k,n,0]+0.000000001) #BGR : 0=Blue, 1=Green, 2=Red
			
			if Green_Blue_division <1.8:
				img_redim[[k],[n]]=0
				img2[[k],[n]]=0
			else:
				img_redim[[k],[n]] = img_redim[[k],[n]]
				img2[[k],[n]]=255
	
	"""separando as canais da imagem e nomeando como R, G e B. Determinando também a média de cada um dos canais	"""
	B, G, R = cv2.split(img_redim) 
	meanG = np.mean(G, axis=None) 
	meanR = np.mean(R, axis=None)
	meanB = np.mean(B, axis=None)
	
	"""calculando os índices vegetativos utilizando as médias de intensiades em cada um dos canais"""
	MPRI_calc = (meanG - meanR)/(meanG + meanR) 
	Gn_calc = (meanG)/(meanG + meanR + meanB) 
	Rn_calc = (meanR)/(meanG + meanR + meanB) 
	Bn_calc = (meanB)/(meanG + meanR + meanB) 
	
	"""vetor res recebe os valores dos quatro índices calculados.
	para cada iteração, os valores em res são armazenados na lista "indices"""""
	res = [MPRI_calc, Gn_calc, Rn_calc, Bn_calc]
	indices.append(res)
	
	"""mostra a imagem sempro processada para conferência visual """
	cv2.imshow("img %d" %(img_number), img_redim)
	cv2.waitKey(10000)
	cv2.destroyAllWindows()
	print(img_number)
	img_number += 1	

"""transforma a lista indices em um DataFrame"""
df = pd.DataFrame(indices, columns = ['MPRI', 'Gn', 'Rn', 'Bn'])
df.index = df.index + 1
"""exportando dados em CSV"""
cvs_file = df.to_csv("C:/Users/lucas/Documents/Python Scripts/Leaf area index/leonardo.csv")
print(df)

cv2.waitKey()
cv2.destroyAllWindows() 