import pandas as pd
import statsmodels.api as sm
import numpy as np
from itertools import combinations
from scipy import stats

# Read the data
df = pd.read_csv("/Users/wenqing_liu/Desktop/第三学期/Regression method/data.csv")
n = len(df)

# Create df1 (heads to heads) - dropping columns 1 and 3 (0-based indexing in Python)
df1 = df.drop(df.columns[[1, 3]], axis=1)

# Create df2 (tails to heads) - dropping columns 0 and 2 (0-based indexing in Python)
df2 = df.drop(df.columns[[0, 2]], axis=1)

# Modify df2 (tails to tails)
df2.iloc[:, 0] = df2.iloc[:, 1] - df2.iloc[:, 0]

# Rename columns for both dataframes
new_columns = ["y", "m", "person", "coin", "group"]
df1.columns = df2.columns = new_columns

start = np.repeat(['heads', 'tails'], [n, n])

# Combine dataframes
df_combined = pd.concat([df1, df2], ignore_index=True)

# Ensure numeric columns are float
df_combined['y'] = pd.to_numeric(df_combined['y'], errors='coerce')
df_combined['m'] = pd.to_numeric(df_combined['m'], errors='coerce')

# Convert columns to categorical
df_combined['person'] = pd.Categorical(df_combined['person'])
df_combined['coin'] = pd.Categorical(df_combined['coin'])
df_combined['group'] = pd.Categorical(df_combined['group'])

# Create response variable (successes and failures)
y = np.column_stack([df_combined['y'], df_combined['m'] - df_combined['y']]).astype(float)

# List of all categorical variables
categorical_vars = ['coin', 'group', 'person']

# Function for performing Likelihood Ratio Test
def lr_test(ll1, ll2, df_diff):
    lr_stat = 2 * (ll2 - ll1)
    p_value = 1 - stats.chi2.cdf(lr_stat, df_diff)
    return lr_stat, p_value

# Function for fitting models
def fit_model(X, y):
    try:
        model = sm.GLM(y, X, family=sm.families.Binomial())
        results = model.fit()
        return results
    except:
        print(f"Warning: Model fitting failed, variable combination: {X.columns.tolist()}")
        return None

# Create DataFrame to store results
results_df = pd.DataFrame(columns=['Model', 'Variables', 'Deviance', 'DF', 'AIC', 'LogLik'])

# Start with baseline model (only constant)
X_base = sm.add_constant(np.ones(len(df_combined))).astype(float)
model_base = fit_model(X_base, y)
ll_base = model_base.llf if model_base is not None else 0

print("Baseline Model (Only Constant):")
print("===================")
if model_base is not None:
    print(model_base.summary())
    # Store baseline model results
    results_df.loc[len(results_df)] = {
        'Model': 'Baseline Model',
        'Variables': 'const',
        'Deviance': model_base.deviance,
        'DF': model_base.df_resid,
        'AIC': model_base.aic,
        'LogLik': model_base.llf
    }

# Dictionary to store model results
model_results = {}

# Loop through all combinations of categorical variables
for num_vars in range(1, len(categorical_vars) + 1):
    for combo in combinations(categorical_vars, num_vars):
        combo_name = ' + '.join(combo)
        print(f"\nModel (Constant + {combo_name}):")
        print("===================")
        
        # Create design matrix
        X = pd.get_dummies(df_combined[list(combo)], drop_first=True).astype(float)
        X = sm.add_constant(X)
        
        # Fit model
        model = fit_model(X, y)
        
        if model is not None:
            print(model.summary())
            ll = model.llf
            
            # Store model results
            results_df.loc[len(results_df)] = {
                'Model': f'Model {len(results_df)}',
                'Variables': combo_name,
                'Deviance': model.deviance,
                'DF': model.df_resid,
                'AIC': model.aic,
                'LogLik': model.llf
            }
            
            # Perform LR test with previous model
            if num_vars > 1:
                # Get previous combination
                prev_combo = combo[:-1]
                prev_ll = model_results[' + '.join(prev_combo)]['ll']
                prev_X = model_results[' + '.join(prev_combo)]['X']
                
                lr_stat, p_value = lr_test(prev_ll, ll, df_diff=X.shape[1] - prev_X.shape[1])
                print(f"\nLikelihood Ratio Test with Previous Model:")
                print(f"LR Statistic: {lr_stat:.4f}")
                print(f"p-value: {p_value:.4f}")
            
            # Store results
            model_results[combo_name] = {
                'll': ll,
                'X': X,
                'model': model,
                'deviance': model.deviance,
                'df': model.df_resid
            }

# Save results to CSV
results_df.to_csv('model_comparison_results.csv', index=False)

# Print summary table
print("\nModel Comparison Results:")
print("===================")
print(results_df.to_string())

# Print unique values
print("\nCategories of each variable:")
print("===================")
for var in categorical_vars:
    print(f"Categories of {var}:", df_combined[var].unique())
