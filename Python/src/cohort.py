from typing import List, Tuple
from collections import namedtuple

import pandas as pd

from .steps import Pipeline


fields = ['desc', 'n_input', 'n_criterion', 'n_excluded', 'n_left']
AttritionItem = namedtuple('AttritionItem', fields)


class SelectionCriterion(Pipeline):
    """Single cohort selection criterion, either used as inclusion or exclusion criterion
    
    Args:
        desc: short readable description of the selection criterion, e.g., "aged < 18 years"
        type: Type of selection criterion. Defaults to "exclusion".
    """
    def __init__(self, desc: str, type: str = "exclusion") -> None:
        super().__init__(desc)
        self._applied = False
        self.type = type

    @property
    def selected(self) -> pd.DataFrame:
        """Selected observations after applying the criterion

        Raises:
            ValueError: If the criterion was not been applied to data yet.

        Returns:
            _description_
        """
        if not self._applied:
            raise ValueError('SelectionCriterion must be applied first')
        return self._selected

    @property
    def n(self) -> int:
        """Number of selected observations

        Returns:
            _description_
        """
        return self.selected.shape[0]

    def apply(self, input=None) -> pd.DataFrame:
        """Apply all transformation steps sequentially to obtain a list of patients that 
        fulfil the criterion.

        Args:
            input: Optional input to the first step, if required. All subsequent steps
             receive the output of the previous step. Defaults to None.

        Returns:
            pd.DataFrame with a single column `stay_id` of all selected observations.
        """
        res = super().apply(input)
        self._applied = True
        self._selected = res[['stay_id']]
        return self.selected
    
    def __repr__(self) -> str:
        repr = f"<SelectionCriterion[{self.type}]>: {self.desc}"
        repr += f" [n={self.n}]" if self._applied else ""
        repr += "\n"
        for i, step in enumerate(self.steps):
            repr += f"   {i+1}. {str(step)}\n"
        return repr


class Cohort():
    """Cohort definition 

    Args: 
        population: total patient population from which to select the cohort according
            to the selection criteria
    """
    def __init__(self, population: pd.DataFrame) -> None:
        self.population = population.copy()
        self.criteria = []
    
    def add_criterion(self, criterion: SelectionCriterion | List):
        """Add one or more criteria to the cohort

        Args:
            criterion: _description_
        """
        if isinstance(criterion, SelectionCriterion):
            criterion = [criterion]
        self.criteria += criterion

    def select(self) -> Tuple[pd.DataFrame, pd.DataFrame]:
        """Apply the selection criteria to the patient population

        Raises:
            TypeError: if criteria other than "inclusion" or "exclusion" are given

        Returns:
            the selected subset of the total patient population,
            an attrition table
        """
        population = self.population
        attrition = []
        for criterion in self.criteria:
            criterion.apply()
            
            if criterion.type == "exclusion":
                new_pop = population.merge(criterion.selected, how='left', on='stay_id', indicator=True)
                new_pop = new_pop[new_pop._merge == "left_only"]
                new_pop.drop('_merge', axis=1, inplace=True)
            elif criterion.type == "inclusion":
                new_pop = population.merge(criterion.selected, how='inner', on='stay_id')
            else:
                raise TypeError(f'Only SelectionCriterion of type "inclusion" or "exclusion" allowed, got {criterion.type}.')
            
            # Log the attrition
            attrition.append(AttritionItem(
                desc=criterion.desc,
                n_input=population.shape[0],
                n_criterion=criterion.n,
                n_excluded=population.shape[0]-new_pop.shape[0],
                n_left=new_pop.shape[0]
            ))

            population = new_pop

        attrition = pd.DataFrame(attrition)
        return population, attrition    