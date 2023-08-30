import pandas as pd
import rpy2
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri

source = ro.r['source']
as_data_frame = ro.r['as.data.frame']
as_null = ro.r['as.null']

def r_to_pandas(df: ro.RObject) -> pd.DataFrame:
    """Convert an R data.frame to pandas.DataFrame

    Args:
        df: R data.frame to convert

    Returns:
        converted pandas.DataFrame
    """
    with (ro.default_converter + pandas2ri.converter).context():
        return replace_r_nas(ro.conversion.get_conversion().rpy2py(df))
    
def replace_r_nas(df: pd.DataFrame) -> pd.DataFrame:
    """Replace special R NA types with standard pandas NA

    Args:
        df: an R data.frame that was recently converted to pandas

    Returns:
        data with only standard pandas NA
    """
    for col in df.columns:
        df[col] = df[col].apply(lambda val: pd.NA if isinstance(val, rpy2.rinterface_lib.sexp.NACharacterType) else val)
    return df