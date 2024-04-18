from sklearn.metrics import mean_squared_error 
from prophet import Prophet
import pandas as pd
import numpy as np
import scipy
import properscoring
import matplotlib.pyplot as plt
import seaborn as sb
import time


sb.set_theme(style='whitegrid')

data_path = './pm25_2009_2019_golden_horseshoe_sites.csv'
data = pd.read_csv(data_path)

data['ds'] = pd.to_datetime(data[['year', 'month', 'day']])

prophet_type = 0 # 0 - standard, 1 - bayesian
prophet_type_s = 'Standard' if prophet_type == 0 else 'Bayesian'
beta = 0.05

mse_l = []
crps_l = []
pic_l = []
is_l = []
training_time_l = []

print(f'Running Prophet with type: {prophet_type_s}')
for col in data.columns[0:12]:
    df = data.loc[:, [col, 'ds']]
    df.rename(columns={col:'y'}, inplace=True)
    df_train = df[(df['ds']<'2019-1-1')]
    df_test = df[df['ds']>'2018-12-31'].reset_index(drop=True)
    if prophet_type == 0:
        # pt = Prophet(interval_width=1-beta, uncertainty_samples=1000, n_changepoints=48, changepoint_range=0.95, yearly_seasonality=12, weekly_seasonality=False, daily_seasonality=4)
        pt = Prophet(interval_width=1-beta, uncertainty_samples=1000, n_changepoints=49, changepoint_range=0.95, yearly_seasonality=12, weekly_seasonality=7, daily_seasonality=False)
        start = time.monotonic()
        pt.fit(df_train)
        stop = time.monotonic()
    else:
        # pt = Prophet(mcmc_samples=500, interval_width=1-beta, uncertainty_samples=1000, n_changepoints=48, changepoint_range=0.95, yearly_seasonality=12, weekly_seasonality=False, daily_seasonality=4)
        pt = Prophet(mcmc_samples=500, interval_width=1-beta, uncertainty_samples=1000, n_changepoints=49, changepoint_range=0.95, yearly_seasonality=12, weekly_seasonality=7, daily_seasonality=False)
        start = time.monotonic()
        pt.fit(df_train, show_progress=False)
        stop = time.monotonic()
    training_time_l.append(stop-start)
    pred = pt.make_future_dataframe(periods=df_test.shape[0])
    forecast = pt.predict(pred)

    # for residuals
    # df_pred = forecast[forecast['ds']<'2019-01-01']
    # residuals = df_train['y'] - df_pred['yhat']
    # residuals.to_csv(f'{col}_residuals.csv')
    # residuals.plot().get_figure().savefig(f'./plots/{col}_{prophet_type_s}_residuals.png', dpi=200)

    # forecast from model
    df_pred = forecast[forecast['ds']>'2018-12-31'].reset_index(drop=True)

    # Writing pred data to file
    df_pred.to_csv(f'./output/pred_{col}_{prophet_type_s}.csv', index=False)

    # plots
    pt.plot(forecast).savefig(f'./plots/{col}_{prophet_type_s}_overall.png', dpi=200)
    pt.plot_components(forecast).savefig(f'./plots/{col}_{prophet_type_s}_components.png', dpi=200)
    plt.close('all')

    correlation = scipy.signal.correlate(df_test['y'], df_pred['yhat'])
    p = sb.lineplot(data=correlation)
    p.figure.savefig(f'./plots/{col}_{prophet_type_s}_correlation.png', dpi=150)

    # metrics
    mse = mean_squared_error(y_true=df_test['y'], y_pred=df_pred['yhat'])
    # print(f'MSE for station {col}: {mse}')
    mse_l.append(mse)

    crps = properscoring.crps_gaussian(x=df_test['y'], mu=df_pred['yhat'].mean(), sig=df_pred['yhat'].std()).mean()
    # print(f'CRPS for station {col}: {crps}')
    crps_l.append(crps)
    
    pic_count = 0
    pic = 0
    for i in range(df_pred.shape[0]):
        if (df_test.loc[i, 'y'] > df_pred.loc[i, 'yhat_lower']) and (df_test.loc[i, 'y'] < df_pred.loc[i, 'yhat_upper']):
            pic_count += 1
    pic = pic_count / df_pred.shape[0]
    pic_l.append(pic)

    interval_score = 0
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
    is_l.append(interval_score)

print('\n--------------------')
print('Currrent data: PM25')
print(f'Current beta value: {beta}')
print(f'Currrent Prophet type: {prophet_type_s}')
print(f'Model training time: {np.sum(training_time_l):0.2f} s')
print(f'Average MSE value: {np.mean(mse_l):0.2f}')
print(f'Average CRPS value: {np.mean(crps_l):0.2f}')
print(f'Average PIC value: {np.mean(pic_l):0.2f}')
print(f'Average Interval score: {np.mean(is_l):0.2f}')
print(f'Standard deviation for MSE: {np.std(mse_l):0.2f}')
print(f'Standard deviation for CRPS: {np.std(crps_l):0.2f}')
print(f'Standard deviation for PIC: {np.std(pic_l):0.2f}')
print(f'Standard deviation for Interval score: {np.std(is_l):0.2f}')
