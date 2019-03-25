#!/usr/bin/env python2.7
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved. 
# For further details please refer to the file COPYRIGHT.txt 
# which you should have received as part of this distribution. 
# *****************************COPYRIGHT******************************* 
"""
Script to process the results of a suite and write a summary to file. The
summary is in Trac wiki mark-up.

This is modified from the UM version with permission from Glenn Greed to
include it in the JULES distribution.

Owner: Kerry Smout-Day
Syntax: shutdown handler = "/path/to/suite_report.py"
"""

import glob
import os
import re
import sqlite3
import sys
from datetime import datetime
from collections import OrderedDict


class ExternalCommandError(Exception): pass
def run_command(command, shell=False):
    '''Given a command as a string, run it and return stdout.
       If the command fails, an exception is thrown with stderr as the message.
       The optional shell argument allows a shell to be spawned to allow 
       multiple commands to be run.'''

    import subprocess

    if shell:
        # Create the Popen object and connect out and err to pipes using
        # the shell=True option.
        pobj = subprocess.Popen(command, stdout=subprocess.PIPE,
                                         stderr=subprocess.PIPE, shell=True)
    else:
        # Turn command into a list
        command_list = command.split()
  
        # Create the Popen object and connect out and err to pipes
        pobj = subprocess.Popen(command_list, stdout=subprocess.PIPE,
                                              stderr=subprocess.PIPE)
                                       
    # Do the communicate and wait to get the results of the command before returning
    stdout, stderr = pobj.communicate()
    if pobj.wait() != 0:
        raise ExternalCommandError(stderr)
    else:
        return stdout


def get_suite_info(suite_path):
    """Gets general information about the suite"""
    
    # Suite name is easy
    name = os.path.basename(suite_path)
    
    # Parse the rose-suite-run.conf for the site and groups
    site = groups = None
    with open(os.path.join(suite_path, "log", "rose-suite-run.conf")) as rsr_file:
        # Don't read all the lines into memory first...
        for line in rsr_file:
            match = re.search(r'SITE\s*=\s*(.*)', line)
            if match:
                site = match.group(1).strip("'\"")
                continue
            match = re.search(r'RUN_NAMES\s*=\s*(.*)', line)
            if match:
                # Groups is an array of strings, so we parse that syntax
                groups = [group.strip(" '\"") for group in match.group(1).strip(' []').split(',')]
                continue
            match = re.search(r'CYLC_VERSION\s*=\s*(.*)', line)
            if match:
                 cylc_version = match.group(1)
                 continue
            match = re.search(r'ROSE_VERSION\s*=\s*(.*)', line)
            if match:
                 rose_version = match.group(1)
                 continue

    # Get the start and end time from the suite log
    start = end = duration = None
    # Pattern match exactly the iso-datetime styled data
    date_pattern = re.compile(r".*?(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z).*?")

    with open(os.path.join(suite_path, "log", "suite", "log")) as log_file:
        for line in log_file:
            # Only look for the start time if we haven't already found it
            if start is None:

                # Then you just need to identify a key phrase in the line
                if "Suite starting" in line:
                    match = date_pattern.search(line)
                    if match:
                        start = datetime.strptime(match.group(1), "%Y-%m-%dT%H:%M:%SZ")

            # Only look for the end time if we already found the start time
            else:
                if "Suite shutting down" in line:
                    match = date_pattern.search(line)
                    if match:
                        end = datetime.strptime(match.group(1), "%Y-%m-%dT%H:%M:%SZ")
                        break

    if start is None:
        start = 'unavailable'

    if end is None:
        end = 'unavailable'
        duration = 'unavailable'      
    else:
        duration = end - start
    
    return { 'name' : name, 'site' : site, 'groups' : groups, 
             'start' : start, 'end' : end, 'duration' : duration, 
             'cylcv' : cylc_version, 'rosev' : rose_version }


def get_source_info(suite_path):
    """Gets information about the source used for the test"""
    
    # Parse the jules-0.version file to get this information
    source_tree = branch = revision = uncommitted_change = None
    changes = []
    local_changes = False
    with open(os.path.join(suite_path, "log", "jules-0.version")) as version_file:
        for line in version_file:
            if re.search(r'SVN STATUS', line, re.IGNORECASE):
                local_changes = True
            # Match starting with A|C|D|I|M|R|X|?|! and one whitespace
            match = re.search(r'^[ACDIMRX?!]\s(.*)', line)
            if match:
                uncommitted_change = match.group(1).strip()
                changes.append(uncommitted_change)
            match = re.search(r'^Working Copy Root Path:\s*(.*)', line)
            if match:
                source_tree = match.group(1).strip()
                continue
            match = re.search(r'^URL:\s*(.*)', line)
            if match:
                branch = match.group(1).strip()
                continue
            match = re.search(r'^Last Changed Rev:\s*(\d+)', line)
            if match:
                revision = match.group(1).strip()
                continue
            
    # Get the parent url
    # If the branch is a shared repo url, we want to return the parent as a
    # shared repo url, even though we may only be able to discover it via the mirror
    # If the branch is a mirror url, we just return parent as a mirror url
    # If getting the parent fails on a mirror url, we just assume there isn't one
    if is_shared_url(branch):
        # Try to get the parent using the shared repo
        parent = get_parent_url(branch)
        # If it failed, try to get the parent using the mirror
        if parent is None:
            mirror_url = to_mirror_url(branch)
            # We can only try the mirror url if it exists
            if mirror_url:
                parent = get_parent_url(mirror_url)
                # If we found it successfully, translate it to a shared url
                if parent:
                    parent = to_shared_url(parent)
    else:
        parent = get_parent_url(branch)
    
    return { 'source_tree' : source_tree, 'branch' : branch,
             'revision' : revision, 'local_changes' : local_changes, 
             'parent' : parent, 'changes' : changes}
    
    
def get_parent_url(svn_url):
    """Given an svn url, tries to find the parent of that url
       If no parent can be found, None is returned"""
       
    try:
        command = "fcm branch-info %s" % svn_url
        for line in run_command(command).splitlines():
            match = re.search(r'Branch Parent:\s*(.*)', line)
            if match:
                return match.group(1).strip()
        return None
    except ExternalCommandError:
        # If there is an error executing the command, return None
        return None


def get_task_states(suite_path):
    '''Returns a dictionary of task states'''

    suite_db_file = os.path.join(suite_path, 'cylc-suite.db')
    db = sqlite3.connect(suite_db_file)
    cursor = db.cursor()
    cursor.execute('select name, status from task_states;') 
    data = {}
    for row in cursor:
        data[row[0]] = row[1]
    db.close()
    return data


# The shared prefix is always the same
__PREFIXES = None
def get_repo_prefixes():
    """Gets the prefixes for the mirror and shared repositories using fcm kp"""
    
    global __PREFIXES
    
    # Compute the prefixes if we haven't already
    if __PREFIXES is None:
        # The shared repo prefix is always the same
        __PREFIXES = { 'shared' : 'https://code.metoffice.gov.uk/svn/jules/main' }
        # Try and find a mirror prefix using fcm kp
        for line in run_command("fcm kp").splitlines():
            match = re.search(r'location\{primary\}\[jules\.xm\]\s*=\s*(.*)', line)
            if match:
                __PREFIXES['mirror'] = match.group(1).strip()
                continue
    
    return __PREFIXES.get('shared'), __PREFIXES.get('mirror')


def is_mirror_url(svn_url):
    """Returns true if the svn url is a mirror url, false otherwise"""
    
    shared_prefix, mirror_prefix = get_repo_prefixes()
    return False if mirror_prefix is None else svn_url.startswith(mirror_prefix)
    

def to_mirror_url(svn_url):
    """Takes an svn url and corrects it for an internal mirror repository
       If there is no internal mirror repository, None is returned"""
       
    shared_prefix, mirror_prefix = get_repo_prefixes()
    
    if mirror_prefix is None:
        return None
    if svn_url.startswith(mirror_prefix):
        return svn_url
    return svn_url.replace(shared_prefix, mirror_prefix)


def is_shared_url(svn_url):
    """Returns true if the svn url is a shared url, false otherwise"""
    
    shared_prefix, mirror_prefix = get_repo_prefixes()
    return False if shared_prefix is None else svn_url.startswith(shared_prefix)
    

def to_shared_url(svn_url):
    """Takes an svn url and corrects it for the shared repository
       If the url is already a shared repo url, it is returned, 
       otherwise it is assumed to be an internal mirror url and converted"""
       
    shared_prefix, mirror_prefix = get_repo_prefixes()
    
    if shared_prefix is None:
        return None
    if svn_url.startswith(shared_prefix):
        return svn_url
    return svn_url.replace(mirror_prefix, shared_prefix)


def to_trac_link(svn_url, revision = None):
    """Converts an svn url and revision to a Trac wiki link
       This is always a link to the shared repo Trac, even if the svn url
       is a mirror url
       Revision can either be given as @xxxx on the end of the svn url, or as
       an additional argument, or not at all
       If the svn url contains @xxxx, that overrides any argument value"""
    
    # Make sure that we have a shared repository url before converting to a link
    svn_url = to_shared_url(svn_url)

    # See if there is a revision to parse out of the url
    if '@' in svn_url:
        svn_url, revision = svn_url.split('@')

    link = re.sub(r'svn', r'trac', svn_url)
    elements = link.split('/')
    elements.insert(elements.index('trac') + 2, 'browser')
    link = "/".join(elements)
    if revision:
        link = "%s?rev=%s" % (link, revision)
    
    return link


def dict_to_wiki_table(data, titles = None):
    """Return an array containing the lines of wiki markup for a table representation of a dictionary
       Optionally, a mapping of keys in data to titles can be given, otherwise the keys are used
       The iteration order of titles (or data if not present) determines the order
       of rows in the wiki table"""
       
    # Note - in order to make sure that we maintain the iteration order from data
    #        when titles is not given, we must use an ordereddict for our stand-in titles
    if titles is None:
        titles = OrderedDict()
        for k in data:
            titles[k] = k
        
    lines = []
    for key, title in titles.iteritems():
        lines.append(" ||%s||%s|| " % (title, data[key]))
    return lines


if __name__ == '__main__':

    # Find the suite database file
    if 'CYLC_SUITE_RUN_DIR' in os.environ:
        suite_path = os.environ['CYLC_SUITE_RUN_DIR']
    elif len(sys.argv) > 1:
        suite_path = sys.argv[1]
    else:
        sys.exit("Unable to find suite path")
        
    suite_path = os.path.realpath(suite_path)


    # Output suite information first
    trac_output = [ "'''Suite Information'''" ]
    data = get_suite_info(suite_path)
    # groups should be a comma separated string instead of a list
    data['groups'] = ", ".join(data['groups'])
    # Since the order in the wiki table is determined by the iteration order
    # of titles, we use an ordered dict to make this predictable
    # Note also that we can control the alignment of our titles and values
    # by adding spaces to the front and/or back of the strings
    titles = OrderedDict()
    titles['name'] = " '''Suite name'''"
    titles['site'] = " '''Site'''"
    titles['groups'] = " '''Groups run'''"
    titles['cylcv'] = " '''Cylc Version'''"
    titles['rosev'] = " '''Rose Version'''"
    titles['start'] = " '''Start time'''"
    titles['end'] = " '''End time'''"
    titles['duration'] = " '''Duration'''"
    trac_output.extend(dict_to_wiki_table(data, titles))
    trac_output.append('')
    
    # Next, output information about the source used
    trac_output.append("'''Source Information'''")
    data = get_source_info(suite_path)
    # Convert branch url to a Trac link, but keep the url itself as the text
    data['branch'] = "[%s %s]" % (to_trac_link(data['branch'], data['revision']), data['branch'])
    # We want local changes to be Yes/No rather than True/False
    # We also want to highlight if there are local changes by making the Yes bold-italic
    data['local_changes'] = "'''''Yes'''''" if data['local_changes'] else "No"
    # If there is no parent, flag that using bold-italic
    # Otherwise, create a Trac link similar to branch above
    if data['changes'] == []:
        data['changes'] = None
    else:
        data['changes'] = "%s %s" % ("'''WARNING !!! uncommitted changes included are:'''", data['changes'])
    if data['parent'] is not None:
        data['parent'] = "[%s %s]" % (to_trac_link(data['parent']), data['parent'])
    else:
        data['parent'] = "'''''Unable to locate branch parent'''''"
    titles = OrderedDict()
    titles['source_tree'] = " '''Source tree used'''"
    titles['branch'] = " '''Branch URL'''"
    titles['revision'] = " '''Revision'''"
    titles['local_changes'] = " '''Local changes?'''"
    titles['changes'] = " "
    titles['parent'] = " '''Branch parent'''"
    trac_output.extend(dict_to_wiki_table(data, titles))
    trac_output.append('')

    # Append the testing results
    trac_output.append("'''Test Results'''")
    # Table titles
    trac_output.append(" ||= '''Task''' =||= '''Status''' =||")
    data = get_task_states(suite_path)
    # Filter out housekeeping jobs
    data = { t : s for t, s in data.iteritems() if not t.startswith('housekeep') }
    # Highlight any non-succeeded tasks
    for task, state in data.iteritems():
        if not 'succeeded' in state:
            data[task] = "'''''%s'''''" % state
    # Put the tasks in alphabetical order
    # Due to the naming conventions, this means similar tasks will be together
    data = OrderedDict(sorted(data.items()))
    trac_output.extend(dict_to_wiki_table(data))

    # Output to file
    with open(os.path.join(suite_path, 'trac.log'), 'w') as file:
        file.writelines(map(lambda x: x + "\n", trac_output))
