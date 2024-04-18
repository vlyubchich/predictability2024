from sklearn.metrics import mean_squared_error 
from prophet import Prophet
import pandas as pd
import numpy as np
import scipy
import properscoring
import matplotlib.pyplot as plt
import seaborn as sb
import timeit


sb.set_theme(style='whitegrid')

data_path = './WeatherStation.csv'
data = pd.read_csv(data_path)
data['ds'] = pd.to_datetime(data[['year', 'month', 'day', 'hour']])

prophet_type = 0 # 0 - standard, 1 - bayesian
prophet_type_s = 'Standard' if prophet_type == 0 else 'Bayesian'
col = 'windspeedmph'
col_s = 'Wind speed'

beta = 0.05

df = data.loc[:, [col, 'ds']]
df = df[11: :24].reset_index(drop=True)
df.rename(columns={col:'y'}, inplace=True)
df_train = df[df['ds']<'2021-09-22 13:00:00']
df_test = df[df['ds']>'2021-09-23 11:00:00'].reset_index(drop=True)

print(f'Running Prophet with type: {prophet_type_s}')
if prophet_type == 0:
    pt = Prophet(interval_width=1-beta, uncertainty_samples=1000, n_changepoints=49, changepoint_range=0.95, yearly_seasonality=12, weekly_seasonality=7, daily_seasonality=False)
    start = timeit.default_timer()
    pt.fit(df_train)
    stop = timeit.default_timer()
else:
    pt = Prophet(mcmc_samples=500, interval_width=1-beta, uncertainty_samples=1000, n_changepoints=49, changepoint_range=0.95, yearly_seasonality=12, weekly_seasonality=7, daily_seasonality=False)
    start = timeit.default_timer()
    pt.fit(df_train, show_progress=False)
    stop = timeit.default_timer()
pred = pt.make_future_dataframe(periods=df_test.shape[0])
forecast = pt.predict(pred)

df_pred = forecast[forecast['ds']>'2021-09-22 12:00:00'].reset_index(drop=True)

# Writing pred data to file
df_pred.to_csv(f'./output/pred_{col}_{prophet_type_s}.csv', index=False)

# residuals = df_test['y'] - df_pred['yhat']
# p = sb.lineplot(data=residuals)
# p.figure.savefig(f'./plots/{col}_residuals.png', dpi=150)

correlation = scipy.signal.correlate(df_test['y'], df_pred['yhat'])
p = sb.lineplot(data=correlation)
p.figure.savefig(f'./plots/{col}_{prophet_type_s}_correlation.png', dpi=150)

pt.plot(forecast).savefig(f'./plots/{col}_{prophet_type_s}_overall.png', dpi=100)
pt.plot_components(forecast).savefig(f'./plots/{col}_{prophet_type_s}_components.png', dpi=100)
plt.close('all')

interval_score = 0
if prophet_type==0:
    for i in range(df_pred.shape[0]):
        true_value = df_test.loc[i, 'y']
        pred_u = df_pred.loc[i, 'yhat_upper']
        pred_l = df_pred.loc[i, 'yhat_lower']
        # term 1
        term1 = pred_u - pred_l
        # term 2
        if true_value < pred_l:
            term2 = (2/beta) * (pred_l - true_value)
        else:
            term2 = 0
        # term 3
        if true_value > pred_u:
            term3 = (2/beta) * (true_value - pred_u)
        else:
            term3 = 0        
        interval_score += (term1 + term2 + term3)
    interval_score = (0.5 * beta * interval_score) / (df_test.shape[0])

mse = mean_squared_error(y_true=df_test['y'], y_pred=df_pred['yhat'])

crps = properscoring.crps_gaussian(x=df_test['y'], mu=df_pred['yhat'].mean(), sig=df_pred['yhat'].std()).mean()

pic_count = 0
pic = 0
if prophet_type==0:
    for i in range(df_pred.shape[0]):
        if (df_test.loc[i, 'y'] > df_pred.loc[i, 'yhat_lower']) and (df_test.loc[i, 'y'] < df_pred.loc[i, 'yhat_upper']):
            pic_count += 1
    pic = pic_count / df_pred.shape[0]

print('\n--------------------')
print(f'Currrent data: {col_s}')
print(f'Current beta value: {beta}')
print(f'Currrent Prophet type: {prophet_type_s}')
print(f'Model training time: {(stop-start):0.2f} s')
print(f'MSE value: {mse:0.2f}')
print(f'CRPS value: {crps:0.2f}')
print(f'PIC value: {pic:0.2f}')
print(f'Interval score: {interval_score:0.2f}')
