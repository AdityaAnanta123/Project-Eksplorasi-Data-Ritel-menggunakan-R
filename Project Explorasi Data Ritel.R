#Load data and save in variable named ‘data’
data <- read.csv("https://storage.googleapis.com/dqlab-dataset/transaksi_stok_dan_penjualan.tsv", header = TRUE, sep = "\t")

#Show top 5 data 
head(data,5)

#Show bottom 5 data
tail(data,5)

#Show information about structure data
str(data)

#Convert type of column "Tanggal" to date using function as.Date()
data$Tanggal <- as.Date(data$Tanggal, "%d-%m-%Y")

#Check if the column "Tanggal" is convert to type date
str(data$Tanggal)

#Add new column "Bulan_Tahun" to save month and year data 
data$Bulan_Tahun <- format(data$Tanggal, "%m-%Y")

#Show 5 top data
head(data, 5)

#COnvert type data in column "Harga" to numeric using function as.numeric()
data$Harga <- as.numeric(data$Harga)

#Convert missing values (NA) to 0 
data$Harga[is.na(data$Harga)] <- 0

#Check if type from column "Harga" is convert to type numeric
str(data$Harga)

#Show 5 top data
head(data, 5)

#1 Study Case Monthly Sales

#Make a new variable data_penjualan which contains values "Jenis.Transaksi == Penjualan"
data_penjualan = data[data$Jenis.Transaksi=="Penjualan",]

#Using anggregate function to get monthly sales  
penjualan_perbulan = aggregate(x=data_penjualan$Jumlah, 
                               by = list(Bulan_Tahun = data_penjualan$Bulan_Tahun),
                               FUN = sum)

#Visualize variable "penjualan_perbulan" using bar plot
barplot(penjualan_perbulan$x,
        names.arg =penjualan_perbulan$Bulan_Tahun,
        xlab="Month",
        ylab="Penjualan",
        col="blue",
        main="Penjualan perbulan",
        border="red")


#2. Study case sorting customer
#Make a new variable data_penjualan which contains values "Jenis.Transaksi == Penjualan"
data_penjualan = data[data$Jenis.Transaksi=="Penjualan",]

#Using aggregate function to get purchases per customer
pembelian_pelanggan=aggregate(
  x=data_penjualan$Jumlah,
  by = list(Pelanggan =data_penjualan$Nama.Pelanggan),
  FUN = sum)

#Sorting customer data based on total purchases from the highest to the lowest
pembelian_pelanggan = pembelian_pelanggan[order(-pembelian_pelanggan$x), ]

#Show top 10 data from "pembelian_pelanggan"
head(pembelian_pelanggan, 10)

#3. Study Case Comparison between "Penjualan" and "Stok_Masuk"
#Comparison of incoming and outcoming goods per month 
aggregate(
  x=data$Jumlah, 
  by = list(Bulan = data$Bulan_Tahun, Jenis_Transaksi = data$Jenis.Transaksi), 
  FUN = sum)

#Make a new variabel named "data_transaksi" using anggregate function
data_transaksi = aggregate(
  x=data$Jumlah, 
  by = list(Bulan = data$Bulan_Tahun, Jenis_Transaksi = data$Jenis.Transaksi), 
  FUN = sum)

#Make a new variable named "data_penjualan" and "data_stok_masuk" that contains values except "Penjualan" and "Stok Masuk"
data_penjualan <- data_transaksi[(data_transaksi$Jenis_Transaksi) == "Penjualan",]
data_stok_masuk <- data_transaksi[(data_transaksi$Jenis_Transaksi) == "Stok Masuk",]

#Merge two variable using merge() function with left join
data_gabungan = merge(data_stok_masuk,data_penjualan,by='Bulan', all.x=TRUE)
data_gabungan = data.frame(Bulan = data_gabungan$Bulan,
                           Stok_Masuk = data_gabungan$x.x,
                           Penjualan = data_gabungan$x.y)

#Check if data have missing values (NA). If in data have missing values that contains values with 0
data_gabungan$Penjualan[is.na(data_gabungan$Penjualan)] <- 0

#Change format "data_gabungan" using transpose command. Next, change name column using month
data_gabung = t(as.matrix(data_gabungan[-1]))
colnames(data_gabung) = data_gabungan$Bulan

#Visualize multiple category with bar plot to compare "stok_masuk" and "penjualan". Next show legend from barplot
barplot(data_gabung,
        main='Perbandingan Penjualan dengan Stok Masuk',
        ylab='Jumlah Barang', 
        xlab='Bulan',
        beside = TRUE, 
        col=c("red","blue"))
legend('topright',fill=c("red","blue"),legend=c('Stok Masuk','Penjualan'))


#4. Study Case Analysis correlation between "Harga_Barang" and "Jumlah_Transaksi"
#Choose data with ""Jenis.Transaksi == Penjualan"
data <- data[(data$Jenis.Transaksi) == "Penjualan",]

#Convert data "Harga" to integer using as.integer()
data$Harga <- as.integer(data$Harga)

#Change missing values(NA) into values 0
data$Harga[is.na(data$Harga)] <- 0

#Count "No.Transaksi" based on "Harga"
data_transaksi <- aggregate(
  x=data$No.Transaksi, 
  by = list(Harga = data$Harga), 
  FUN = length)

#Sorting data based on expensive price
data_transaksi = data_transaksi[order(-data_transaksi$Harga), ]

#Visualize data correlation between "Harga" and "Data.Trasaksi". Before visualize using hist(), we should convert data into a vector
data_transaksi_freq = as.vector(rep(data_transaksi$Harga, data_transaksi$x))

#Visualize using hist() function to visualize correlation between "Harga" and "Transaksi"
hist(data_transaksi_freq,
     main="Hubungan antara harga barang dengan transaksi",
     xlab="Rentang harga barang",
     col="green"
)
