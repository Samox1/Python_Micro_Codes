import matplotlib.pyplot as plt
import pandas as pd
import numpy as np


def plot_canceled(df):
    df_canceled = df[df.is_canceled]

    labels = ['# customers', '# orders', '# products']
    revenue = [df.customer_id.nunique(), df.order_id.nunique(), df.product_id.nunique()]
    profit = [df_canceled.customer_id.nunique(), df_canceled.order_id.nunique(), df_canceled.product_id.nunique()]

    x = np.arange(len(labels))  
    width = 0.35  


    fig, ax = plt.subplots(figsize=(15, 5))
    rects1 = ax.bar(x - width/2, revenue, width, label='All')
    rects2 = ax.bar(x + width/2, profit, width, label='Canceled')

    # Add some text for labels, title and custom x-axis tick labels, etc.
    ax.set_ylabel('Procent')
    ax.set_title('Profit* = Revenue - Cancelation')
    ax.set_xticks(x)
    ax.set_xticklabels(labels)
    ax.legend()

    def autolabel(rects):
        """Attach a text label above each bar in *rects*, displaying its height."""
        for rect in rects:
            height = rect.get_height()
            ax.annotate('{}'.format(height),
                        xy=(rect.get_x() + rect.get_width() / 2, height),
                        xytext=(0, 3),  # 3 points vertical offset
                        textcoords="offset points",
                        ha='center', va='bottom')


    autolabel(rects1)
    autolabel(rects2)

    fig.tight_layout()
    plt.show()
    

def my_round(value, n):
    return int(value * 10**n) / 10**n
    
def get_cumsum(df, main_feat, price_feat):
    df_price = df.groupby(main_feat).agg(np.sum)[price_feat].sort_values(ascending=False)
    total_price = df_price.sum()

    return ( df_price.cumsum() / total_price ).mul(100)

def get_top(df, main_feat, price_feat, sel_x_prct):
    df_cumsum = get_cumsum(df, main_feat, price_feat)
    n_df = df[main_feat].nunique()
    sel_x_n = n_df
    
    if 'auto' in sel_x_prct:
        cut_prct = int( sel_x_prct.split('_', 1)[1] )
        sel_x_n = df_cumsum[ df_cumsum < cut_prct ].shape[0]
        sel_x_prct =  sel_x_n / n_df
        sel_x_prct = my_round(sel_x_prct, 4)
    else:
        sel_x_n = int(n_df * sel_x_prct)
        
    return set(list(df_cumsum.head(sel_x_n).index))
    
def plot_top(df_cumsum, sel_x_prct, sel_x_n, sel_y_value, item_x_label, item_y_label, base_title=''):
    #add zero point
    df_cumsum = pd.Series( [0] + list(df_cumsum.values) )
    
    plt.figure(figsize=(15, 5), dpi=200)
    plt.plot(df_cumsum.index, df_cumsum.values)
    x_one_perct = df_cumsum.index.max() / 100

    title = "{}\n{}% {}, {}% {}".format(base_title, sel_x_prct * 100, item_x_label, sel_y_value, item_y_label)
    plt.title(title, fontsize=13)
    plt.xlabel('Procent {}'.format(item_x_label))
    plt.ylabel('Procent {}'.format(item_y_label))
    
    step_x, step_y = 5, 10
    plt.xticks([int(x*x_one_perct) for x in range(0, 101, step_x)], [x for x in range(0, 101, step_x)], rotation=90)
    plt.yticks([x for x in range(0, 101, step_y)], [x for x in range(0, 101, step_y)])

    plt.axvline(linewidth=2, ymin=0, ymax=1, x=sel_x_n, color='green');




def plot_pareto(df, main_feat, price_feat, sel_x_prct='auto_80', base_title=''):
    x_labels = {
        'product_id': 'produktów',
        'customer_id': 'klientów',
        'order_id': 'zamówień'
    }
    y_labels = {
        'price_total': 'przychodów' if df['is_canceled'].nunique() == 2 else 'zwrotów',
        'profit_total': 'zysków'
    }
    item_x_label = x_labels[main_feat]
    item_y_label = y_labels[price_feat]
    
    df_cumsum = get_cumsum(df, main_feat, price_feat)
    n_df = df[main_feat].nunique()
    sel_x_n = n_df
    
    if 'auto' in sel_x_prct:
        cut_prct = int( sel_x_prct.split('_', 1)[1] )
        sel_x_n = df_cumsum[ df_cumsum < cut_prct ].shape[0]
        sel_x_prct =  sel_x_n / n_df
        sel_x_prct = my_round(sel_x_prct, 3)
    else:
        sel_x_n = int(n_df * sel_x_prct)
    
    #print("sel_x_prct={}, sel_x_n={}, n_df={}".format(sel_x_prct, sel_x_n, n_df))
    print("Unikalnych {}: {}".format(item_x_label, n_df))
    print("{}% to {} {}".format(sel_x_prct * 100, sel_x_n, item_x_label))
    sel_y_value = my_round( df_cumsum.head(sel_x_n).tail(1).values[0], 1)
    
  
    args = {
        'df_cumsum': df_cumsum, 
        'sel_x_prct':sel_x_prct, 
        'sel_x_n': sel_x_n,
        'sel_y_value': sel_y_value, 
        'item_x_label': item_x_label, 
        'item_y_label': item_y_label, 
    }

    plot_top(**args)