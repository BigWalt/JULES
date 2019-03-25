######################################################################
## This file contains the main benchmarking program
##
## It is designed to be run on the command line
######################################################################

import os, shutil, sys, traceback
import jinja2, numpy

# Import the benchmarking code
# We must first add our lib directory to the python path
source_path = os.path.realpath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.append(os.path.join(source_path, 'python', 'lib'))
import bench


######################################################################
# Parse the given options
######################################################################
parser = bench.log.LoggingArgumentParser(description='JULES benchmarking tool')
parser.add_argument('-d', '--data-dir', metavar = "DATADIR", default = "./data",
                    help = "Path to benchmarking data directory")
parser.add_argument('-o', '--output-dir', metavar = "OUTDIR", default = "./output",
                    help = "Path to model output directory")
parser.add_argument('-r', '--report-dir', metavar = "REPORTDIR", default = "./report",
                    help = "Directory to create report in")
parser.add_argument('-t', '--traceback', action='store_true', default=False,
                    help="Print stack traces when an exception occurs")    
# Title is a required argument
parser.add_argument('title', metavar = "TITLE", help = "Title for the report")
args = parser.parse_args()
    

# Catch any errors in the rest of the code so we can report them nicely
try:
    bench.log.info("Creating report directory")
    # Remove the current report directory and make an empty one
    if os.path.exists(args.report_dir):
        shutil.rmtree(args.report_dir)
    os.mkdir(args.report_dir)
    # Make a json directory within the report directory
    json_dir = os.path.join(args.report_dir, 'json')
    os.mkdir(json_dir)
    
    
    ######################################################################
    # Create the report object
    ######################################################################
    bench.log.info("Running tests")
    
    report = {
        'title' : args.title,
        'sections' : []
    }
    report['sections'].append(bench.tests.fluxnet.evap(args.data_dir, args.output_dir, json_dir))
    report['sections'].append(bench.tests.fluxnet.gpp(args.data_dir, args.output_dir, json_dir))
    report['sections'].append(bench.tests.fluxnet.rsp(args.data_dir, args.output_dir, json_dir))
    report['sections'].append(bench.tests.lai.lai(args.data_dir, args.output_dir, json_dir))
    report['sections'].append(bench.tests.frac.frac(args.data_dir, args.output_dir, json_dir))
    report['sections'].append(bench.tests.closures.closures(args.data_dir, args.output_dir, json_dir))
    
    
    ######################################################################
    # Render the report
    ######################################################################
    bench.log.info("Generating report")
    
    # Create the Jinja2 environment we will use
    # Our templates are in the html directory that sits at the same level
    # as the python directory containing this script
    jinja_env = jinja2.Environment(
        loader = jinja2.FileSystemLoader(os.path.join(source_path, 'templates'))
    )
    
    # Register custom filters
    jinja_env.filters['slugify'] = bench.custom_filters.slugify
    
    # Get the root template
    template = jinja_env.get_template("report.html")
    
    # Run the template and save it to file
    with open(os.path.join(args.report_dir, "report.html"), 'wb') as file:
        file.write(template.render(report = report))
    
    # Copy the assets folder to the report directory
    shutil.copytree(
        os.path.join(source_path, 'assets'),
        os.path.join(args.report_dir, 'assets')
    )
    
    bench.log.done("Generated report in %s" % args.report_dir)


except Exception, e:
    if args.traceback:
        bench.log.error(traceback.format_exc())
    bench.log.fatal(str(e))
