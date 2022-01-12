install.packages("readxl")
install.packages("dplyr")
library("dplyr")
library("readxl")
library("qdap")


#Load the data into "data"
data <- read_xlsx("C:/Users/VIVEK KUMAR SHARMA/OneDrive/Desktop/TISS/Software Class/R Studio/Assignment mini project/Supply_Use_Table_2015-16.xlsx",sheet=2)

data[is.na(data)] <- 0

#Change dataframe into matrix
matrix <- as.matrix(data)

#STRUCTURE OF DATA

#find top 5 values from matrix
head(matrix)


#type of matrix
class(matrix)


#find summary of the matrix
summary(matrix)


#fill the vacant or NA value with 0, so make it in proper matrix form
matrix[is.na(matrix)] <- 0


#find dimension of the matrix
dim(matrix)


                      #Agriculture Sector Analysis


#Total GDP of Agriculture sectors

AGRIC <- matrix[1:140, 3]
AGRIC <- as.numeric(AGRIC)
AGR <- sum(AGRIC)
AGR

TOTAL <- matrix[1:140, 3:67]
TOTAL <- as.numeric(TOTAL)
TOTAL_GDP <- sum(TOTAL)
TOTAL_GDP

#Agriculture percentage
(AGR/TOTAL_GDP)*100


#Total Industry product in Agriculture sector
data %>% count(Agriculture)


                  #SECTORAL ANALYSIS

#Divide the Data-set into 3 categories: Primary, Secondary & Tertiary sectors


#Primary to Primary
PRI <- matrix[1:40, 3:11]
PRI <- as.numeric(PRI)
P_to_P <- sum(PRI)
P_to_P


#Primary vs Secondary sectors

PR_SEC <- matrix[1:40, 12:40]
PR_SEC <- as.numeric(PR_SEC)
P_to_S <- sum(PR_SEC)


#Primary vs Tertiary sectors
PR_TER <- matrix[1:40, 41:67]
PR_TER <- as.numeric(PR_TER)
P_to_T <- sum(PR_TER)


#Secondary to Secondary
SEC <- matrix[41:112, 12:40]
SEC <- as.numeric(SEC)
S_to_S <- sum(SEC)


#Secondary sectors VS Primary 

SEC_PRI <- matrix[41:112, 3:11]
SEC_PRI <- as.numeric(SEC_PRI)
S_to_P <- sum(SEC_PRI)


#Secondary to Tertiary
SEC_TER <- matrix[41:112, 41:67]
SEC_TER <- as.numeric(SEC_TER)
S_to_T <- sum(SEC_TER)


#Tertiary to Primary
TER_PRI <- matrix[113:140, 3:11]
TER_PRI <- as.numeric(TER_PRI)
T_to_P <- sum(TER_PRI)


#Tertiary to Secondary
TER_SEC <- matrix[113:140, 12:40]
TER_SEC <- as.numeric(TER_SEC)
T_to_S <- sum(TER_SEC)


#Tertiary to Tertiary
TER <- matrix[113:140, 41:67]
TER <- as.numeric(TER)
T_to_T <- sum(TER)

#Input Output Matrix
IOM <- matrix(c(P_to_P, P_to_S, P_to_T, 
                S_to_P, S_to_S, S_to_T, 
                T_to_P, T_to_S, T_to_T), 
              nrow = 3, ncol = 3, byrow = TRUE)

rownames(IOM) <- c("PRIMARY", "SECONDARY", "TERTIARY")
colnames(IOM) <- c("PRIMARY", "SECONDARY", "TERTIARY")
IOM


#Primary Final Demand
PR_F<-matrix[1:40, 68]
PR_F<-as.numeric(PR_F)
P_Final <- sum(PR_F)


#Secondary Final Demand          
SEC_F<-matrix[41:112, 68]
SEC_F<-as.numeric(SEC_F)
S_Final<-sum(SEC_F)


#Tertiary Final Demand
TER_F<-matrix[113:140, 68]
TER_F<-as.numeric(TER_F)
T_Final<-sum(TER_F)


#Final Demand matrix for Primary, Secondary and Tertiary Sectors
matrix_F <- matrix(c(P_Final, S_Final, T_Final), nrow = 3, ncol = 1)
rownames(matrix_F) <- c("PRIMARY (FINAL Demand)", "SECONDARY (FINAL Demand)", "TERTIARY (FINAL Demand)")
matrix_F


#Primary Total Output
PR_T <- matrix[1:40, 74]
PR_T <- as.numeric(PR_T)
Primary_Total <- sum(PR_T)
Primary_Total


#Secondary Total Output
SEC_T <- matrix[41:112, 74]
SEC_T <- as.numeric(SEC_T)
Secondary_Total <- sum(SEC_T)


#Tertiary Total Output
TER_T <- matrix[113:140, 74]
TER_T <- as.numeric(TER_T)
Tertiary_Total <- sum(TER_T)


#Total Output Matrix(X) for Primary, Secondary and Tertiary Sectors
matrix_X <- matrix(c(Primary_Total,Secondary_Total,Tertiary_Total), nrow = 3, ncol = 1)
rownames(matrix_X) <- c("PRI (Total Output)", "SEC (Total Output)", "TER (Total Output)")
matrix_X


#Matrix(A) - Aggregate Technology Matrix
u11 <- (P_to_P/Primary_Total)
u12 <- (P_to_S/Secondary_Total)
u13 <- (P_to_T/Tertiary_Total)

u21 <- (S_to_P/Primary_Total)
u22 <- (S_to_S/Secondary_Total)
u23 <- (S_to_T/Tertiary_Total)

u31 <- (T_to_P/Primary_Total)
u32 <- (T_to_S/Secondary_Total)
u33 <- (T_to_T/Tertiary_Total)

matrix_A <- matrix(c(u11, u12, u13, u21, u22, u23, u31, u32, u33), nrow = 3, ncol = 3, byrow = TRUE)
matrix_A


#Proving the equation: AX + F = X
LHS <- (matrix_A %*% matrix_X) + matrix_F
rownames(LHS) <- c("Pri(Total Output)", "Sec(Total Output)", "Ter(Total Output)")
RHS <- matrix_X

LHS
RHS
