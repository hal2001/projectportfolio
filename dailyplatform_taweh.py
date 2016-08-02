import os 
import pandas as pan
import pyodbc 
import numpy as np
import datetime

#########################################################################
#Directory Change 
os.chdir('C:\\Users\\Taweh.Beysolow\\Desktop')
#Masterfile containing all data 
country_map = pan.read_csv('BBCountrytoRegionMap.csv', header = None, delimiter = ',')

countrymap_df  = country_map.loc[:, 0:1]
countrymap_df.columns  = ['Code', 'Name']
currencies = pan.read_csv('MasterFileUpd.csv', header = None, delimiter = ',')
masterfile = pan.read_csv('masterfilefinalcopy.csv', header = 0, delimiter = ',')
masterfile.loc[:, 'ID'] = masterfile.loc[:, 'ID'].apply(lambda x: str(x).upper())
masterfile.loc[:, 'AXYS ID'] = masterfile.loc[:, 'AXYS ID'].apply(lambda x: str(x).upper())
rcg_risk = pan.read_csv('RCGRef.csv', header = 0, delimiter = ',')

db_match = pan.DataFrame(rcg_risk.loc[202:463, :])
db_match = db_match.reset_index(drop = 'index')

db_match = db_match.loc[:, ['Category1','Category2', 'Category3', 'Category4']]
db_match['Category2'] = db_match['Category2'].apply(lambda x: str(x).upper())
db_match['Category3'] = db_match['Category3'].apply(lambda x: str(x).upper())
db_match['Category4'] = db_match['Category4'].apply(lambda x: str(x).upper())
#########################################################################
#Establishing Connection To Server
conn = pyodbc.connect('DRIVER={SQL SERVER};SERVER=10.13.0.25;UID=Transparency;PWD=RCGTransparency;TDS_Version=8.0;PORT=1433;') 
curs = conn.cursor()

#Getting Data From Database
curs.execute('''select * from research.dbo.vdailytrades
WHERE AsOfDate = '2016-07-28'
''')

df = pan.DataFrame([list(r) for r in curs.fetchall()], columns=[h[0] for h in curs.description])
df.loc[:, 'SecuritySymbol'] = df.loc[:, 'SecuritySymbol'].apply(lambda x: str(x).upper())
#########################################################################
# Data taken from Masterfile
currencies = pan.DataFrame(currencies[9][52321:52378], dtype = str)
currencies = currencies.reset_index(drop = 'index')
currencies[9] = currencies[9].apply(lambda x: str(x).upper())
#########################################################################
#Finding All Portfolios
########################################################################
portfolio_names = pan.DataFrame(np.unique(df.loc[:, 'PortfolioCode']))
###################################################################
#All the portfolio names
###################################################################
fund_ids = portfolio_names[1:len(portfolio_names)]
########################################################
#Finding out which securities are new
def newHoldings(symb):
    Id = pan.DataFrame(masterfile.loc[:, 'ID'])
    new_holdings = []
    
    for i in range(0, len(symb)):
            if (symb[i] not in Id.values):
                new_holdings.append(symb[i])
    
    new_holdings = pan.DataFrame(np.unique(new_holdings))
    
    return new_holdings
    
newholdings = pan.DataFrame(newHoldings(df.loc[:, 'SecuritySymbol']))

newholdings.to_csv('newholdings.csv')
########################################################
#Capitalization Categorization Algorithm 
########################################################  
def convertTickers(tickers):
    
    for i in range(0, len(tickers)):
     
         if (tickers[i]  in currencies.values):        
            tickers[i] = 'USD Curncy'

    tickers = pan.DataFrame(tickers)
    
    return tickers                    
########################################################  
#Converting from coutry code to 
def convertCountry(code):
    output = []
    na = "Country Not Available"
    
    for u in range(0, len(code)):
        if(pan.isnull(code.values[u]) == True):
                output.append(na)
        for i in range(0, len(countrymap_df)):
            if(code.values[u] == countrymap_df.loc[i, 'Code']):
                output.append(countrymap_df.loc[i, 'Name'])
            
    if (len(output) == 0):
        output.append(na)
            
    return output

############################################################################
#Calculating Portfolio Values
#############################################################################
def portfolioValue(fund_id):
    
    #Organizing Inputs into Data Frame     
    value = pan.DataFrame(df.loc[:,'MarketValue'].values, dtype = float)
    port = pan.DataFrame(df.loc[:, 'PortfolioCode'].values, dtype = str)
    data = pan.concat([port, value], axis = 1)
    data.columns = ['PortfolioCodes', 'MarketValue']
    
    List = list()
    
    val = []
       
      
    for i in range(0, len(df)):
        
        if (data.loc[i,'PortfolioCodes'] == fund_id):
        
                val.append(df.loc[i, 'MarketValue'])
            
    val = pan.DataFrame(val, dtype = float)
    val = np.sum(val)
    List.append(val)
    
    return List
##################################################################
#Calculating the weight of equity within a given portfolio 
#Almost all equations derive from this . I recommend checking here should there be any 
#issues with the script
##################################################################
def weightCalc(fund_id):
    
    value = pan.DataFrame(df.loc[:,'MarketValue'].values, dtype = float)
    port = pan.DataFrame(df.loc[:, 'PortfolioCode'].values, dtype = str)
    sec = pan.DataFrame(df.loc[:, 'SecuritySymbol'].values, dtype = str)
    sec[0] = sec[0].apply(lambda x: str(x).upper())

    data = pan.concat([port, value, sec], axis = 1)
    data.columns = ['PortfolioCodes', 'MarketValue', 'SecuritySymbol']

    database =  df[df['PortfolioCode']== fund_id][['SecuritySymbol', 'MarketValue']].groupby(['SecuritySymbol']).sum() 
    total = np.sum(database.loc[:, 'MarketValue'])
    sym_val = pan.DataFrame(database.values)
    sym_name = pan.DataFrame(database.index)
    sym_name['SecuritySymbol'] = sym_name['SecuritySymbol'].apply(lambda x: str(x).upper())
    sym_df = pan.concat([sym_name, sym_val], axis = 1)
    
    df2 = data.merge(masterfile,  how = 'left', left_on = 'SecuritySymbol', right_on = 'AXYS ID')   
    df2 = df2[df2['PortfolioCodes']== fund_id][['ID', 'SecuritySymbol']]
    df2 = df2.reset_index(drop = 'index')

    #Creating Dictionaries
    symbol_dict = {}
    val_dict = {}    
    
    for i in range(0, len(df2)):
        symbol_dict[df2.loc[i, 'SecuritySymbol']] = df2.loc[i, 'ID']    
    
    
    for i in range(0, len(sym_name)):        
        val_dict[sym_df.loc[i,'SecuritySymbol']] = sym_df.loc[i, 0]
        
    equities = []
    val = []
    cash = []
    cashsymb = 'USD CURNCY'
     
    for u in range(0, len(sym_df)):
        for i in range(0, len(symbol_dict)):
            if (sym_df.loc[u, 'SecuritySymbol'] == symbol_dict.keys()[i] and sym_df.loc[u, 'SecuritySymbol'] not in currencies.values):
                equities.append(symbol_dict.values()[i])  
                val.append(sym_df.loc[u,0])       
            elif(sym_df.loc[u, 'SecuritySymbol'] == val_dict.keys()[i] and sym_df.loc[u, 'SecuritySymbol'] in currencies.values):
                cash.append(val_dict.values()[i])
    
    
    equities.append(cashsymb)
    equities = pan.DataFrame(equities)
    
    cash = np.nansum(cash) 
    val.append(cash)
    val = pan.DataFrame(val)
    total = np.nansum([float(i) for i in val[0]])
    val = pan.DataFrame(([float(i) for i in val[0]]))
    val = val/total
    output = pan.concat([equities, val], axis = 1)
    output.columns = ['Security', 'Measure']

    return output
########################################################
#################################################################
#Long/Short Exposure Calculation 
#Condition: If the equity weight is positive, then it is  long position 
#Else, it is a short position 
#################################################################
def long_shortExp(weights):
    
    long_short = list()
    L = 'Long'
    S = 'Short'
    
    for i in range(0, len(weights)):
        
        if (weights.values[i] > 0):
    
            long_short.append(L)
           
        else: 
            
            long_short.append(S)
    
    long_short = pan.DataFrame(long_short)
   
    return long_short
#################################################################
#Economic Long/Short Exposure based on Long/Short Positions of Equities Within Portfolios 
#Condition: If the position is long, the equity has economic long exposure 
#else, it has economic short exposure 
#################################################################
def ecoExposure(weights):
    
    exposure = list()
    el = 'Economic Long Exposure'
    es = 'Economic Short Exposure'
    g = '% Gross Exposure'
    
    for i in range(0, len(weights)):
        
        
        if (weights.values[i] == 'Long'):
            exposure.append(el)
        elif (weights.values[i] == 'Short'):
            exposure.append(es)
        elif (weights.values[i] == 'Gross'):
            exposure.append(g)
            
    exposure = pan.DataFrame(exposure)

    return exposure
###############################################################################
#Tier Leveel: Denotes the amoutn of Exposure a specific equity has
###############################################################################
def tierLevel(weights):
        
    tier = list()
    null = ""
    exp = 'Tier 1 - Exposure'
    
    for i in range(0, len(weights)):
        
        if (weights.values[i] == ""):
            tier.append(null)
        else:
            tier.append(exp)
            
    tier = pan.DataFrame(tier)
   
    return tier
######################################################################
#Sector Exposure Calculation
######################################################################
def sectorExposure(fund_id):
    
    Slist = []
    data = weightCalc(fund_id)
    df2 = data.merge(masterfile,  how = 'left', left_on = 'Security', right_on = 'ID')    
    fund_data = df2.loc[:, ['Security', 'Measure', 'SECTOR']]    

    d_f = fund_data
    
    sector_list = np.unique(d_f.loc[:, 'SECTOR'])
    sector_list = sector_list[sector_list != 'Other']
    
    #Summing the Weights of the Inidvidual Holdings and Then Converting to Percentage Form    
    for q in range(0, len(sector_list)):
        
            Val = []
            
            for i in range(0, len(d_f)):                    
                if (d_f.loc[i, 'SECTOR'] == sector_list[q] and d_f.loc[i, 'Security'] not in currencies.values):                
                    Val.append(d_f.loc[i, 'Measure'])
            
            Val = pan.DataFrame(np.unique(Val))
                    
            if (len(Val) != 0):                        
                Val = np.nansum([float(z) for z in Val[0]])            
            else:
                Val = 0
                                
            Slist.append(Val)
    
    Slist = pan.DataFrame(Slist, dtype = float)
    sector_list = pan.DataFrame(sector_list, dtype = str)
    sector_list = pan.concat([sector_list, Slist], axis = 1)      

    return sector_list
######################################################################
#Exposure Across Different Market Cap Groups 
######################################################################
def capExposure(fund_id):

    data = weightCalc(fund_id)
    df2 = data.merge(masterfile,  how = 'left', left_on = 'Security', right_on = 'ID')    
    fund_data = df2.loc[:, ['Security', 'Measure', 'MARKET CAP']]
    data_dict = {}
    val_dict = {}
        
    for i in range(0, len(fund_data)):
        data_dict[fund_data.loc[i, 'Security']] = fund_data.loc[i, 'Measure']
        
    for i in range(0, len(fund_data)):
        val_dict[fund_data.loc[i, 'Security']] = float(fund_data.loc[i, 'MARKET CAP'])
    
    data_dict1 = pan.DataFrame(data_dict.keys())
    data_dict2 = pan.DataFrame(data_dict.values())
    data_dict = pan.concat([data_dict1, data_dict2], axis = 1)
    
    val_dict1 = pan.DataFrame(val_dict.keys())
    val_dict2 = pan.DataFrame(val_dict.values())
    val_dict = pan.concat([val_dict1, val_dict2], axis = 1)
    
    data_dict.columns = ['Security', 'Measure']
    val_dict.columns = ['Security', 'MarketCap']
     
    SC = []
    MC = []
    LC = []
       
    
    for i in range(0, len(data_dict)):
        if (val_dict.loc[i, 'Security'] in currencies.values):
            SC.append(data_dict.loc[i, 'Measure'])
        elif (val_dict.loc[i, 'MarketCap'] <= 2065):
            SC.append(data_dict.loc[i, 'Measure'])
        elif (val_dict.loc[i, 'MarketCap'] <= 5893):
            MC.append(data_dict.loc[i, 'Measure'])
        elif(val_dict.loc[i, 'MarketCap'] > 5893):
            LC.append(data_dict.loc[i, 'Measure'])
            
    SC = pan.DataFrame(SC)
    MC = pan.DataFrame(MC)
    LC = pan.DataFrame(LC)
    
    #Summing Data Frames of Respective Weights
    if (len(MC) != 0 ):   
        MC =  np.nansum([float(i) for i in MC[0].values])
    else:    
        MC = 0
    
    if (len(SC) != 0):      
        SC = np.nansum([float(i) for i in SC[0].values])             
    else:
        SC = 0 
  
    if (len(LC) != 0):
        LC =  np.nansum([float(i) for i in LC[0].values])
    else: 
        LC = 0
    
    #Collecting Statistics
    #Once Masterfile is updated with marketcap data, this will show correct data
    output = pan.DataFrame([SC, MC, LC])
    output.index = ['Small Cap', 'Mid Cap', 'Large Cap']
        
    return output
######################################################################
#Exposure Across Different Regions //Problems to be fixed
######################################################################
def regExposure(fund_id):
    
    List = []
    data = pan.DataFrame(weightCalc(fund_id))  
    df2 = data.merge(masterfile,  how = 'left', left_on = 'Security', right_on = 'ID')    
    d_f = df2.loc[:, ['Security', 'Measure', 'COUNTRY']]
    cty =  np.unique(d_f.loc[:, 'COUNTRY'])
    cty = cty[cty != 'Other']
    
    for q in cty:
            
            Val = []
            for i in range(0, len(d_f)):                    
                if (d_f.loc[i, 'COUNTRY'] == q and d_f.loc[i, 'Security'] not in currencies.values):                
                    Val.append(d_f.loc[i, 'Measure'])
            
            Val = pan.DataFrame(np.unique(Val))
                    
            if (len(Val) != 0):                        
                Val = np.nansum([float(z) for z in Val[0]])            
            else:
                Val = 0
                                
            List.append(Val)

    List = pan.DataFrame(List)
    List.index = cty
    
    
    return List     
##################################
#Gross Exposure by Sector
#Finding how much of the portfolio sits within each sector 
###################################     
def grossSector(fund_id):
        
    z = sectorExposure(fund_id)
    z.columns = [0,1]
    total = np.nansum(z.loc[:, 1])
    sec = pan.DataFrame(z.values[:, 1])
    exposure = []
    
    for i in range(len(sec)):
            
        exposure.append(float(sec.loc[i, 0]/total))
        
    exposure = pan.DataFrame(exposure)
    
    exposure.index = z.loc[:, 0]
    
    return exposure 
#####################################
#Gross Exposure by Market Capitalization 
#####################################
def grossCapital(fund_id):

    z = capExposure(fund_id)
    tot = float(np.nansum(z.loc[:, 0]))
    cap = pan.DataFrame(z.values[:, 0])
    per = []
    
    for i in range(len(cap)):
        
        per.append(cap.loc[i,0]/tot)

    per = pan.DataFrame(per)
    per.index = ['Small Cap', 'Medium Cap', 'Large Cap']
    
    return per      
#####################################    
#Gross Exposure by Region
#####################################
def grossRegion(fund_id):

    z = regExposure(fund_id)
    z = pan.DataFrame(z.values, dtype = float)
    z.columns = [0]    
    total = np.nansum(z.loc[:, 0])
    grossreg = []
    
    for i in range(0, len(z)):
        
        grossreg.append(float(z.loc[i,0]/total))
    
    grossreg = pan.DataFrame(grossreg)
    grossreg.index = z.index
    
    return grossreg
#####################################
#Gross Currency
#####################################
def grossCash(fund_id):
    
    tot = pan.DataFrame(sectorExposure(fund_id), dtype = float)
    tot = tot.loc['Total']
    cash = float(1 - tot)
    
    return cash
#####################################
#COnverting Manager Names to Ids 
def convertID(fund_id):
    
    fund_ids = ['rcafr', 'rcfudao', 'rchyun', 'rcjos', 'rcshilla', 'rcpera', 'rcsheng', 'rcdora', 'rcxing', 'rceuro', 
                'rcmena', 'rcshen', 'lakshmi', 'riseuro', 'zhonghua']
                
    nums = ['9787', '9887', '6164', '9349', '12219', '10036', '6288', '9886', '6797', '4833', 
            '11960', '12371', '10367', '12743', '12937']

    fund_ids = pan.DataFrame(fund_ids)
    nums = pan.DataFrame(nums)
    d_f = pan.concat([fund_ids, nums], axis = 1)
    d_f.columns = ['ID', 'Number']    
    
    output = d_f[d_f.loc[:, 'ID'] == fund_id]
    
    output = output.loc[:, 'Number']
    output = int(output)
    
    return output 
    
#Time Functions
def lastday(date):
    if date.month == 12:
        return date.replace(day=31)
    return date.replace(month=date.month+1, day=1) - datetime.timedelta(days=1)

reportdate = lastday(datetime.date(2016, 7, 1))
########################################################################################################
#Daily Output 
########################################################################################################   
def dailyOutput(fund_id):
    
    #inputs
    d_f = pan.DataFrame(weightCalc(fund_id))
    d_f = d_f.reset_index(drop = 'index')    

    Long = []
    Short = []
    ac = 'Asset Category'
    eq = 'Equity'
    pe = 'Public Equity'
    na = ""
    
    for i in range(0, len(d_f)):
        if (d_f.loc[i, 'Measure'] > 0 and d_f.loc[i, 'Security'] != 'USD CURNCY'):
            Long.append(d_f.loc[i, 'Measure'])
        elif (d_f.loc[i, 'Measure'] < 0):
            Short.append(d_f.loc[i, 'Measure'])
   
    Long = np.sum(Long)
    Short = np.sum(Short)
    
    Gross = Long - Short 
    
    index = ['Long', 'Short']
    fund_exposures = pan.DataFrame([Gross, Short])
    fund_index = pan.DataFrame(index)
    fund_index = pan.DataFrame(ecoExposure(fund_index))
    
    cat1 = pan.DataFrame(np.repeat(ac, len(index)))
    cat2 = pan.DataFrame(np.repeat(eq, len(index)))
    cat3 = pan.DataFrame(np.repeat(pe, len(index)))
    cat4 = pan.DataFrame(["Other " + i for i in cat3.values])
    cat5 = pan.DataFrame(np.repeat(na, len(index)))
    tier1 = pan.DataFrame(tierLevel(fund_exposures))    
    reportdate = []
    
    for i in range(0, len(cat5)):
        reportdate.append(lastday(datetime.date(2016, 7, 1)))
    
    reportdate = pan.DataFrame(reportdate)
    
    fund_exposures = pan.concat([reportdate, abs(fund_exposures), tier1, fund_index, cat1 , cat2, cat3, cat4, cat5], axis = 1)
    fund_exposures.columns = [0,1,2,3,4,5,6,7,8]
########################################################################################################
#Fund Specific Data 

    na = ""
    Short = []    
    for i in range(0, len(d_f)):
        if (d_f.loc[i, 'Measure'] < 0 and d_f.loc[i, 'Security'] not in currencies.values):
            Short.append(d_f.loc[i, 'Measure'])
        
          
    Long = Gross
    Gross2 = Gross/Gross
    Short = np.sum(Short)
    
    RLong = Long - Short
    index = ['Gross', 'Long', 'Short']
    tot_exposures = pan.DataFrame([Gross2, RLong, Short])
    fund_index = pan.DataFrame(index)
    tier2 = pan.DataFrame(ecoExposure(fund_index))
    tier1 = pan.DataFrame(tierLevel(tot_exposures))
    cat1 = pan.DataFrame(np.repeat(na, len(tier1)))
    cat2 = pan.DataFrame(np.repeat(na, len(tier1)))
    cat3 = pan.DataFrame(np.repeat(na, len(tier1)))
    cat4 = pan.DataFrame(np.repeat(na, len(tier1)))
    cat5 = pan.DataFrame(np.repeat(na, len(tier1)))
     
    reportdate = []
    
    for i in range(0, len(cat5)):
        reportdate.append(lastday(datetime.date(2016, 7, 1)))
    
    reportdate = pan.DataFrame(reportdate)
    
    tot_exposure = pan.concat([reportdate, abs(tot_exposures), tier1, tier2, cat1, cat2, cat3, cat4, cat5], axis = 1)
    tot_exposure.columns = [0,1,2,3,4,5,6,7,8]
########################################################################################################
#Market Cap Data 
    
    List = []
    cat1 = 'Market Cap'
    cat2 = []
    La = 'Large Cap'
    M = 'Mid Cap'
    Sm = 'Small Cap'
    index = []
    G = 'Gross'
    L = 'Long'
    S = 'Short'
    na = ""
    gross_cap = pan.DataFrame(grossCapital(fund_id))
    cap_levels = pan.DataFrame([2065, 5893, 5893])
    capital_list = pan.DataFrame(gross_cap.index)
    gross_cap = pan.DataFrame(gross_cap.values)
    cap_measures = pan.DataFrame(capExposure(fund_id))
    cap_measures = pan.DataFrame(cap_measures.values)
    data = weightCalc(fund_id)
    df2 = data.merge(masterfile,  how = 'left', left_on = 'Security', right_on = 'ID')    
    fund_data = df2.loc[:, ['Security', 'Measure', 'MARKET CAP']]
    
    capital_df = pan.concat([capital_list, cap_levels], axis = 1)
    capital_df.columns = ['Capital Level', 'Threshold']
       
    for u in range(0, len(capital_df)):
        Short = []
        for i in range(0, len(fund_data)):
            if (fund_data.loc[i, 'Measure'] < 0 and fund_data.loc[i, 'MARKET CAP'] <= capital_df.loc[u, 'Threshold']):
                 Short.append(fund_data.loc[i, 'Measure'])
        Short = pan.DataFrame(Short)
        
        if (len(Short) != 0):
            Short = np.nansum([float(i) for i in Short[0]])
        else:
            Short = 0
            
        List.append(gross_cap.loc[u, 0])
        List.append(cap_measures.values[u,0])
        List.append(Short)
        index.append(G)
        index.append(L)
        index.append(S)
        cat2.append(Sm)
        cat2.append(M)
        cat2.append(La)
    
    cap_exposures = pan.DataFrame(List)
    cap_index = pan.DataFrame(index)
    tier1 = pan.DataFrame(tierLevel(cap_exposures))
    tier2 = pan.DataFrame(ecoExposure(cap_index))
    cat1 = pan.DataFrame(np.repeat(cat1, len(cap_exposures)))
    cat2 = pan.DataFrame(cat2)
    cat3 = pan.DataFrame(np.repeat(na, len(cap_exposures)))
    cat4 = pan.DataFrame(np.repeat(na, len(cap_exposures)))
    cat5 = pan.DataFrame(np.repeat(na, len(cap_exposures)))
   
    reportdate = []
    
    for i in range(0, len(cap_exposures)):
        reportdate.append(lastday(datetime.date(2016, 7, 1)))
    
    reportdate = pan.DataFrame(reportdate)
    
    
    cap_exposures = pan.concat([reportdate, abs(cap_exposures), tier2, tier2,  cat1, cat2, cat3, cat4, cat5], axis = 1)
    cap_exposures.columns = [0,1,2,3,4,5,6,7,8]
########################################################################################################
    
    #Sector Exposures        
    secexp = sectorExposure(fund_id)
    secexp.columns = ['Sector', 'Percentages']
    sec_measures = pan.DataFrame(secexp.loc[:, 'Percentages'], dtype = float)
    grosssec = grossSector(fund_id)
    gross_measures = pan.DataFrame(grosssec.values[:, 0], dtype = float)
    
    #Calculating which sectors have short/long exposure
    value = pan.DataFrame(df.loc[:,'MarketValue'].values, dtype = float)
    port = pan.DataFrame(df.loc[:, 'PortfolioCode'].values, dtype = str)
    sec = pan.DataFrame(df.loc[:, 'SecuritySymbol'].values, dtype = str)
                      
    data = pan.concat([port, value, sec], axis = 1)
    data.columns = ['PortfolioCodes', 'MarketValue', 'SecuritySymbol']
    
    data = data[data.loc[:, 'PortfolioCodes'] == fund_id] 
    df2 = d_f.merge(masterfile,  how = 'left', left_on = 'Security', right_on = 'ID')       
    fund_data = df2.loc[:, ['ID', 'Measure', 'SECTOR', 'COUNTRY']]
       
    sector_list = secexp.loc[:, 'Sector']
    sector_list = pan.DataFrame(sector_list)

    List = []
    index = []
    cat2 = []
    G = 'Gross'
    L = 'Long'
    S = 'Short'
    na = ""
    for u in range(0, len(sector_list)):
               Short = []
               for i in range(0, len(fund_data)):
                   if (fund_data.loc[i, 'Measure'] < 0 and fund_data.loc[i, 'ID'] not in currencies.values and fund_data.loc[i, 'SECTOR'] == sector_list.values[u]):
                             Short.append(fund_data.loc[i, 'Measure'])
                    
               if (len(Short) == 0):
                      Short = 0
              
               Gross = gross_measures.loc[u, 0]
               Long = sec_measures.loc[u, 'Percentages']
               Short = np.nansum(Short)
               Long = Long - Short
               List.append(Gross)               
               List.append(Long)
               List.append(Short)
               index.append(G)
               index.append(L)
               index.append(S)
        
     
    cat2 = pan.DataFrame(np.repeat(sector_list.values, 3))
    cat3 = pan.DataFrame(["Other " + i for i in cat2.values])
    sec_exposures = pan.DataFrame(List)
    sec_index = pan.DataFrame(index)
    
    sec_index = pan.DataFrame(ecoExposure(sec_index)) 
    
    
    cat1 = 'Sector'
    cat1 = pan.DataFrame(np.repeat(cat1, len(sec_exposures)))
    cat2 = pan.DataFrame(cat2)
    cat3 = "Other " + cat2
    cat2 = pan.DataFrame(cat2)
    cat4 = pan.DataFrame(np.repeat(na, len(sec_exposures)))
    cat5 = pan.DataFrame(np.repeat(na, len(sec_exposures)))
    tier1 = pan.DataFrame(tierLevel(sec_exposures))
    reportdate = []
    
    for i in range(0, len(sec_exposures)):
        reportdate.append(lastday(datetime.date(2016, 7, 1)))
    
    reportdate = pan.DataFrame(reportdate)
    sec_exposures = pan.concat([reportdate, abs(sec_exposures), tier1, sec_index, cat1, cat2, cat3, cat4, cat5], axis = 1)
    sec_exposures.columns = [0,1,2,3,4,5,6,7,8]
########################################################################################################    
#Region Exposure    
    
    regexp = pan.DataFrame(regExposure(fund_id))
    regexp.columns = ['Percentages']
    country = pan.DataFrame(regexp.index)
    pct = pan.DataFrame(regexp.loc[:, 'Percentages'])
    pct = pan.DataFrame(pct.values)
    grossreg = grossRegion(fund_id)
    gross_meas = pan.DataFrame(grossreg.values[:, 0], dtype = float)
    
    List = []
    index = []
    G = 'Gross'
    L = 'Long'
    S = 'Short'
    na = ""
    for u in range(0, len(country)):
           Short = []
           Long = []
           for i in range(0, len(fund_data)):
              if (fund_data.loc[i, 'Measure'] < 0 and fund_data.loc[i, 'ID'] not in currencies.values and fund_data.loc[i, 'COUNTRY'] == country.loc[u,0]):
                   Short.append(fund_data.loc[i, 'Measure'])   
           if (len(Short) == 0):
              Short = 0
          
           Gross = gross_meas.loc[u, 0]
           Long = pct.loc[u, 0]
           Short = np.nansum(Short)
           Long = Long - Short
           List.append(Gross)
           List.append(Long)
           List.append(Short)
           index.append(G)
           index.append(L)
           index.append(S)
      
    country = pan.DataFrame(country)
    country = pan.DataFrame(convertCountry(country))
    country[0] = country[0].apply(lambda x: str(x).upper())
    
    cat2 = []
    cat3 = []
    cat4 = []
    cat1 = []
    
    for i in range(0, len(country)):    
        for u in range(0, len(db_match)):
            if(country.loc[i,0] == db_match.loc[u, 'Category4']):
                country.loc[i] = (db_match.loc[u, 'Category3'])
                cat1.append(db_match.loc[u, 'Category1'])
                cat2.append(db_match.loc[u, 'Category2'])
                cat3.append(db_match.loc[u, 'Category3'])
                cat4.append(db_match.loc[u, 'Category4'])
                   
    cat1 = pan.DataFrame(cat1)
    cat2 = pan.DataFrame(cat2)        
    cat3 = pan.DataFrame(cat3)
    cat4 = pan.DataFrame(cat4)
    
    for i in range(0, len(cat4)):
        if (cat4.loc[i,0] == 'UK'):
            cat4.loc[i,0] = na
    cty_exposure = pan.DataFrame(List)
    
       
    cty_index = pan.DataFrame(index[0:len(index)])
    
    tier1 = pan.DataFrame(tierLevel(cty_exposure))
    tier2 = pan.DataFrame(ecoExposure(cty_index))
    length = len(tier2)
    other_length = len(cty_exposure)/len(country)
    
    cat4 = pan.DataFrame(np.repeat(cat4.values, other_length))
    cat2 = pan.DataFrame(np.repeat(cat2.values, other_length))
    cat1 = pan.DataFrame(np.repeat(cat1.values, other_length))
    cat3 = pan.DataFrame(np.repeat(cat3.values, other_length))
    cat5 = pan.DataFrame(np.repeat(na, length))
    
    reportdate = []
    
    for i in range(0, len(tier2)):
        reportdate.append(lastday(datetime.date(2016, 7, 1)))
    
    reportdate = pan.DataFrame(reportdate)
    cty_exposure = pan.concat([reportdate, abs(cty_exposure), tier1, tier2, cat1, cat2, cat3, cat4, cat5], axis  = 1)
    cty_exposure.columns = [0,1,2,3,4,5,6,7,8] 
########################################################################################################    
#Position Data 

    posexp = weightCalc(fund_id)
    posexp = posexp.loc[:, ['Security', 'Measure']]
    
    sec = np.array(posexp.loc[:,'Security'])
    meas = np.array(posexp.loc[:, 'Measure'])
    
    sec = pan.DataFrame(sec)
    meas = pan.DataFrame(meas)
    
    na = ""
    p = 'Positions'
    L_S = pan.DataFrame(long_shortExp(meas))
    pos = pan.DataFrame(np.repeat(p, len(L_S)))
    tier1 = pan.DataFrame(tierLevel(meas))
    cat2 = pan.DataFrame(ecoExposure(L_S))
    
    cat4 = pan.DataFrame(np.repeat(na, len(L_S)))
    cat5 = cat4
    
    reportdate = []
    
    for i in range(0, len(sec)):
        reportdate.append(lastday(datetime.date(2016, 7, 1)))
    
    reportdate = pan.DataFrame(reportdate)
    
    pos_exposure = pan.concat([reportdate, abs(meas), tier1, cat2, pos, L_S, sec, cat4, cat5], axis = 1)    
    pos_exposure.columns = [0,1,2,3,4,5,6,7,8]
    
######################################################################################################## 
#Compiling all of the data into One DataFrame            
    fe = fund_exposures
    te = tot_exposure
    ce = cap_exposures
    se = sec_exposures
    c = cty_exposure
    p = pos_exposure   
    
    output_data = pan.concat([fe, te, ce, se, c, p])
    output_data  = output_data.reset_index(drop = 'index')
    
    #Adding Portfolio Manager Id
    portcode = []
    
    num = convertID(fund_id)
    for i in range(0, len(output_data.loc[:, 0])):
        portcode.append(num)

    portcode = pan.DataFrame(portcode)
    
    
    output_data  = pan.concat([portcode, output_data], axis = 1)
    
    return output_data
######################################################################################################## 
########################################################################################################     
#Running Daily Output Function for All Funds

finaltest = pan.DataFrame()

fund_ids = ['rcafr', 'rcfudao', 'rcjos', 'rcshilla', 'zhonghua', 'lakshmi', 'riseuro', 'rcpera', 'rcdora']

for i in fund_ids:
    ft = pan.DataFrame(dailyOutput(i))
    finaltest = pan.concat([finaltest, ft])

finaltest.to_csv('FinalWeekTest.csv', delimiter = '\t', index = False, header = ['ManagerID', 'ReportDate', 'Measure', 'Tier1', 'Tier2', 'Category1', 
                                                                      'Category2', 'Category3', 'Category4', 'Category5']) 
########################################################################################################       
#End
#Taweh Beysolow II 
    
    
    

    

