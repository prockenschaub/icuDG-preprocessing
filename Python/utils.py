import os
import pandas as pd
import pyarrow.parquet as pq
from pathlib import Path
from sklearn.model_selection import StratifiedShuffleSplit, ShuffleSplit


def output_clairvoyance(data_dir, save_dir, task_type="static"):
    """""Output in clairvoyance format including train test splitting
    Args:
        data_dir: Path to directory where existing parquet data is stored.
        save_dir: Path to directory where output should be saved.
        task_type: Type of task to be performed. Either "static" or "dynamic".

    """

    outc = pq.read_table(os.path.join(data_dir, 'outc.parquet')).to_pandas()
    dyn = pq.read_table(os.path.join(data_dir, 'dyn.parquet')).to_pandas()
    sta = pq.read_table(os.path.join(data_dir, 'sta.parquet')).to_pandas()

    os.makedirs(save_dir, exist_ok=True)
    dyn = dyn.melt(id_vars=['stay_id', 'time'])
    dyn = dyn[~pd.isnull(dyn['value'])]
    data = make_train_test({"static": sta, "dynamic": dyn, "outcome": outc}, task_type=task_type, seed=42, train_size=0.8)
    for key, value in data.items():
        if task_type == "static":
            value["static"] = value["static"].merge(value["outcome"], on='stay_id', how='left')
        else:
            value["dynamic"] = value["dynamic"].merge(value["outcome"], on='stay_id', how='left')
    for key, value in data.items():
        value["static"].to_csv(os.path.join(save_dir, f'static_{key}.csv'), index=False)
        value["dynamic"].to_csv(os.path.join(save_dir, f'dynamic_{key}.csv'), index=False)


def make_train_test(
        data: dict[pd.DataFrame],
        train_size=0.8,
        seed: int = 42,
        task_type: str = "static",
) -> dict[dict[pd.DataFrame]]:
    """Randomly split the data into training and validation sets for fitting a full model.

    Args:
        data: dictionary containing data divided int OUTCOME, STATIC, and DYNAMIC.
        vars: Contains the names of columns in the data.
        train_size: Fixed size of train split (including validation data).
        seed: Random seed.
        debug: Load less data if true.
    Returns:
        Input data divided into 'train', 'val', and 'test'.
    """
    # ID variable
    id = "stay_id"
    label = "label"

    # Get stay IDs from outcome segment
    stays = pd.Series(data["outcome"][id].unique(), name=id)

    # If there are labels, and the task is classification, use stratified k-fold
    if task_type == "static":
        # Get labels from outcome data (takes the highest value (or True) in case seq2seq classification)
        labels = data["outcome"].groupby(id).max()[label].reset_index(drop=True)
        train_test = StratifiedShuffleSplit(train_size=train_size, random_state=seed, n_splits=1)
        train, test = list(train_test.split(stays, labels))[0]
    else:
        # If there are no labels or it is a regression task, use random split
        train_test = ShuffleSplit(train_size=train_size, random_state=seed)
        train, test = list(train_test.split(stays))[0]

    split_ids = {
        "train": stays.iloc[train],
        "test": stays.iloc[test]
    }

    data_split = {"train": {}, "test": {}}
    for split in split_ids.keys():  # Loop through splits (train / val / test)
        # data_split[split] = {"train":{}, "test":{}}
        data_split[split]["static"] = data["static"].merge(split_ids[split], on=id, how="right", sort=True)
        data_split[split]["dynamic"] = data["dynamic"].merge(split_ids[split], on=id, how="right", sort=True)
        data_split[split]["outcome"] = data["outcome"].merge(split_ids[split], on=id, how="right", sort=True)

    # for fold in split.keys():  # Loop through splits (train / val / test)
    #     # Loop through segments (DYNAMIC / STATIC / OUTCOME)
    #     # set sort to true to make sure that IDs are reordered after scrambling earlier
    #     data_split[fold] = {
    #         data_type: data[data_type].merge(split[fold], on=id, how="right", sort=True) for data_type in data.keys()
    #     }
    # # Maintain compatibility with test split
    # data_split[Split.test] = copy.deepcopy(data_split[Split.val])
    return data_split


output_clairvoyance(data_dir=Path(r'C:\Users\Robin\Downloads\demo_data\aki\mimic_demo'),
                    save_dir=Path(r'C:\Users\Robin\Downloads\demo_data\aki\mimic_demo\clairvoyance'), task_type="dynamic")
