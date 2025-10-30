#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.8"
# dependencies = [
#     "pandas>=1.3.0",
#     "numpy>=1.20.0", 
#     "scikit-learn>=1.0.0",
#     "fippy @ git+https://github.com/jemus42/fippy.git@fix-rfi-to-numpy-bug",
# ]
# ///

"""
Calculate feature importance using fippy methods including SAGE and RFI
Save results to JSON for comparison with xplainfi

This version uses uv script dependencies for portable execution
"""

import pandas as pd
import numpy as np
import json
import os
import random
import torch

# Set seeds for reproducibility
random.seed(123)
np.random.seed(123)
torch.manual_seed(123)

def extract_fippy_results(ex_result, feature_names):
    """Extract results from fippy explanation objects"""
    if hasattr(ex_result, 'fi_means_stds'):
        fi_series = ex_result.fi_means_stds()
        
        # The Series contains feature importances and a 'std' entry with another Series
        # Extract just the feature importance values (exclude 'std')
        importance_values = []
        std_values = []
        
        for feature in feature_names:
            if feature in fi_series.index:
                importance_values.append(float(fi_series[feature]))
            else:
                importance_values.append(0.0)
        
        # Try to extract std values if available
        if 'std' in fi_series.index and isinstance(fi_series['std'], pd.Series):
            std_series = fi_series['std']
            for feature in feature_names:
                if feature in std_series.index and not pd.isna(std_series[feature]):
                    std_values.append(float(std_series[feature]))
                else:
                    std_values.append(0.0)
        else:
            std_values = [0.0] * len(feature_names)
        
        return {
            "feature": feature_names,
            "importance": importance_values,
            "std": std_values
        }
    
    return None

def main():
    print("Loading fippy...")
    try:
        from fippy.explainers import Explainer
        from fippy.samplers import GaussianSampler
        from sklearn.ensemble import RandomForestRegressor
        from sklearn.metrics import mean_squared_error
        print("fippy loaded successfully")
    except Exception as e:
        print(f"Error loading fippy: {e}")
        return

    print("Loading data...")
    # Check if pre-split data files exist
    if not os.path.exists("ewald_train.csv") or not os.path.exists("ewald_test.csv"):
        print("Pre-split data files not found. Please run calculate_xplainfi.R first.")
        return
        
    # Load pre-split data to ensure identical train/test sets
    train_data = pd.read_csv("ewald_train.csv")
    test_data = pd.read_csv("ewald_test.csv")
    
    X_train = train_data.drop("y", axis=1)
    y_train = train_data["y"]
    X_test = test_data.drop("y", axis=1)
    y_test = test_data["y"]
    
    print("Training model...")
    # Reset seeds before training
    random.seed(123)
    np.random.seed(123)
    model = RandomForestRegressor(n_estimators=500, random_state=123)
    model.fit(X_train, y_train)

    r2_score = model.score(X_test, y_test)
    print(f"Model R² score: {r2_score:.3f}")

    # Initialize results
    results = {
        "model_performance": {"r_squared": r2_score},
        "feature_names": list(X_train.columns)
    }

    print("Setting up fippy explainer...")
    sampler = GaussianSampler(X_train)
    explainer = Explainer(model.predict, X_train, loss=mean_squared_error, sampler=sampler)
    print("Explainer created")

    # 1. PFI
    print("\nComputing PFI...")
    try:
        # Reset seeds before each method
        random.seed(123)
        np.random.seed(123)
        ex_pfi = explainer.pfi(X_test, y_test, nr_runs=100)
        pfi_results = extract_fippy_results(ex_pfi, list(X_train.columns))
        results["PFI"] = pfi_results
        print("✓ PFI completed")
    except Exception as e:
        print(f"✗ Error computing PFI: {e}")
        results["PFI"] = None

    # 2. CFI
    print("\nComputing CFI...")
    try:
        # Reset seeds before each method
        random.seed(123)
        np.random.seed(123)
        ex_cfi = explainer.cfi(X_test, y_test, nr_runs=100)
        cfi_results = extract_fippy_results(ex_cfi, list(X_train.columns))
        results["CFI"] = cfi_results
        print("✓ CFI completed")
    except Exception as e:
        print(f"✗ Error computing CFI: {e}")
        results["CFI"] = None

    # 3. RFI - Fixed API
    print("\nComputing RFI...")
    try:
        # Reset seeds before each method
        random.seed(123)
        np.random.seed(123)
        # RFI expects: rfi(G, X_eval, y_eval, ...)
        # G is the conditioning set
        conditioning_set = ["x3"]
        ex_rfi = explainer.rfi(conditioning_set, X_test, y_test, nr_runs=100)
        rfi_results = extract_fippy_results(ex_rfi, list(X_train.columns))
        if rfi_results:
            rfi_results["conditioning_set"] = conditioning_set
        results["RFI"] = rfi_results
        print("✓ RFI completed")
    except Exception as e:
        print(f"✗ Error computing RFI: {e}")
        import traceback
        traceback.print_exc()
        results["RFI"] = None

    # 4. Marginal SAGE
    print("\nComputing Marginal SAGE...")
    try:
        # Reset seeds before each method
        random.seed(123)
        np.random.seed(123)
        ex_msage, sage_orderings = explainer.msage(
            X_test, y_test,
            nr_runs=30,
            nr_resample_marginalize = 200,
            detect_convergence=True
        )
        msage_results = extract_fippy_results(ex_msage, list(X_train.columns))
        results["SAGE_Marginal"] = msage_results
        print("✓ Marginal SAGE completed")
    except Exception as e:
        print(f"✗ Error computing Marginal SAGE: {e}")
        results["SAGE_Marginal"] = None

    # 5. Conditional SAGE
    print("\nComputing Conditional SAGE...")
    try:
        # Reset seeds before each method
        random.seed(123)
        np.random.seed(123)
        ex_csage, sage_orderings = explainer.csage(
            X_test, y_test,
            nr_runs=30,
            nr_resample_marginalize = 200,
            detect_convergence=True
        )
        csage_results = extract_fippy_results(ex_csage, list(X_train.columns))
        results["SAGE_Conditional"] = csage_results
        print("✓ Conditional SAGE completed")
    except Exception as e:
        print(f"✗ Error computing Conditional SAGE: {e}")
        results["SAGE_Conditional"] = None

    # Save results
    print("\nSaving results to fippy_results.json...")
    with open("fippy_results.json", "w") as f:
        json.dump(results, f, indent=2)

    print("fippy calculations completed!")
    
    # Print summary
    methods_computed = []
    for method in ["PFI", "CFI", "RFI", "SAGE_Marginal", "SAGE_Conditional"]:
        if results.get(method) is not None:
            methods_computed.append(method)
            if results[method] and "importance" in results[method]:
                print(f"\n{method} Results:")
                for i, feature in enumerate(results[method]["feature"]):
                    importance = results[method]["importance"][i]
                    print(f"  {feature}: {importance:.4f}")
    
    print(f"\nMethods successfully computed: {', '.join(methods_computed)}")

if __name__ == "__main__":
    main()
