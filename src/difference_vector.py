
# coding: utf-8

# In[ ]:

import sqlite3
import pandas as pd
import datetime
from xml.dom import minidom

conn = sqlite3.connect('../database.sqlite')
query_matches =     'select match_api_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, date     from Match;'
matches = pd.read_sql(query_matches, conn, index_col='match_api_id', parse_dates=['date']);


# In[ ]:

def teamattrs(team_api_id, matchdate):
    """
    Helper method that gets the specified team's attributes from
    the date closest to matchdate.
    
    Keyword arguments:
    team_api_id -- the api_id of the team of interest (Integer)
    matchdate -- pd.Timestamp
    """
    query_attrs =         'select date, buildUpPlaySpeed, buildUpPlayPassing,                 chanceCreationPassing, chanceCreationCrossing, chanceCreationShooting,                 defencePressure, defenceAggression, defenceTeamWidth         from Team_Attributes         where team_api_id = %s         order by date asc;' % team_api_id
    attrs = pd.read_sql(query_attrs, conn, parse_dates=['date'])
    if (not attrs.empty):
        closestattrs = None
        mindiff = None
        for index, row in attrs.iterrows():
            date = row['date']
            diff = abs(matchdate - date)
            if (mindiff is None or diff < mindiff):
                mindiff = diff
                closestattrs = row
        return closestattrs.drop('date')
    else:
        return None
    
    
def get_winner(match):
    """
    Given a pd.Series representing a match, returns the 
    team_api_id of the winning team.
    If the match was a draw, returns None.
    """
    if match['home_team_goal'] > match['away_team_goal']:
        return match['home_team_api_id']
    elif match['home_team_goal'] < match['away_team_goal']:
        return match['away_team_api_id']
    else:
        return None

def get_percentages(encounters, team_api_id):
    """
    Given a pd.DataFrame of encounters between two teams,
    extracts the win percentage and possession_percentage
    of the specified team.
    
    For each match, if it was a draw, counts it as a
    win for *both* teams (and so increases the total count
    by 2).
    
    If there were no encounters between the two teams,
    returns -1.
    """
    total_count = len(encounters.index)
    total_pos_count = total_count
    results = {}
    if total_count == 0:
        results['win_percentage'] = -1
        results['pos_percentage'] = -1
        return results
    
    wins = 0
    total_pos = 0
    for (name, item) in encounters.iterrows():
        winner = get_winner(item)
        if winner is None:
            wins = wins + 1
            total_count = total_count + 2
        elif winner == team_api_id:
            wins = wins + 1
        
        elemstring = ''
        if item['home_team_api_id'] == team_api_id:
            elemstring = 'homepos'
        else:
            elemstring = 'awaypos'
        
        possession = item['possession']
        if (possession is not None):
            xmldoc = minidom.parseString(possession)
            values = xmldoc.getElementsByTagName('value')
            if len(values) > 0:
                value = values[len(values) - 1]
                poselem = value.getElementsByTagName(elemstring)
                if poselem:
                    pos = poselem[0].firstChild.nodeValue
                    total_pos = total_pos + int(pos)
                else:
                    total_pos_count = total_pos_count - 1
            else:
                total_pos_count = total_pos_count - 1
        else:
            total_pos_count = total_pos_count - 1
        
        
    results['win_percentage'] =  wins / total_count
    if total_pos_count > 0:
        results['pos_percentage'] = (total_pos / total_pos_count) / 100
    else:
        results['pos_percentage'] = -1
    
    return results


# In[ ]:

def differencevector(row):
    """
    This function is applied to each row in the 'matches' DataFrame.
    For each row, it returns a pandas.Series containing the number of
    goals scored by each team and the result from the home team's
    perspective.
    
    Keyword arguments:
    row -- pandas.Series
    """
    date = row['date']
    homegoal = row['home_team_goal']
    homeattrs = teamattrs(row['home_team_api_id'], date)
    
    awaygoal = row['away_team_goal']
    awayattrs = teamattrs(row['away_team_api_id'], date)
    
    match_query =         "select date, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, possession         from `Match`         where date < '%s' and (home_team_api_id in ('%s', '%s') and (away_team_api_id in ('%s', '%s')))" %         (str(row['date']) + ' 00:00:00', row['home_team_api_id'], row['away_team_api_id'], row['home_team_api_id'], row['away_team_api_id'])
    encounters = pd.read_sql(match_query, conn, parse_dates=['date'])
    
    percentages = get_percentages(encounters, row['home_team_api_id'])
    
    if (homeattrs is not None and awayattrs is not None):
        differencevector = homeattrs - awayattrs

        if (homegoal > awaygoal):
            result = 2
        elif (awaygoal == homegoal):
            result = 1
        else:
            result = 0

        results = pd.Series({
            'outcome':  result,
            'win_percentage': percentages['win_percentage'],
            'pos_percentage': percentages['pos_percentage']
        })
        return results.append([differencevector])
    else:
        return

results = matches.apply(differencevector, axis=1).dropna(axis=0, how='any')
conn.close()

results.to_csv('../difference_vectors_new.csv')

