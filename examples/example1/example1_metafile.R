# First, set up the aqueduct flow and define paths using aqueduct_setup()
aqueduct_setup(
  basepath = "C:/Users/hab737/GitHub/aqueduct/examples/example1",
  raw      ~ basepath/data/raw,
  derived  ~ basepath/data/derived,
  current  ~ basepath/data/current,
  plots    ~ basepath/output/plots,
  projname = "hello"
)
# Then run the aqueduct workflow!
aqueduct(
  raw() ~ create_data(,seed=1996)
  derived(chars) ~ load_chars(raw(location_file) +
                              raw(race_file) +
                              raw(age_file) +
                              raw(education_file)),
  derived(clean_chars) ~ clean_chars(derived(chars)),
  derived(db_w_chars) ~ add_chars(raw(main_db) + derived(clean_chars)),
  current(classified_groups) ~ svm_classify(derived(db_w_chars)),
  plots(classify_plots) ~ create_plots(current(classified_groups) +
                                       derived(clean_chars)),
  output(final_report)  ~ produce_report() 
)
