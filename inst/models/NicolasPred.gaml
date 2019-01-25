model LVAgent

global torus: true {
  float predation_distance_cal;
  float interacting_rate <- 0.0;
  list<float> interacting_rate_values <- [];
  int currentSeed <- 0;
  float calibrate(int nb_type1_init, int nb_type2_init, float pred_rate,float pred_dist  )
  {
    predation_distance_cal <- pred_dist;
    create type1 number: nb_type1_init ;
    create type2 number: nb_type2_init ;

    int nb_step <- 0;

    loop while: nb_step < 30
    {
      ask type1
      {
        do basic_move;
      }
      ask type2
      {
        do basic_move;
      }
      do die_prey;
      nb_step <- nb_step + 1;
    }

    float my_micro_b <- pred_rate / mean(interacting_rate_values);

    ask type1
    {
      do die;
    }

    ask type2
    {
      do die;
    }

    return my_micro_b;

  }

  action die_prey {
    ask type1
    {
      do isInDanger;
    }

    list<type1> inDangerPrey <- type1 where (each.is_in_danger = true) ;
    int sumPredator <- sum(inDangerPrey collect(each.nb_type2_could_kill_me));
    interacting_rate <- sumPredator / (length(type1) * length(type2));
    interacting_rate_values <- interacting_rate_values + interacting_rate;

    ask type1
    {
      is_in_danger <- false;
    }
  }


  float size_of_territory ;
  geometry shape <- square(size_of_territory);

  list<int> prey_population <-[];
  float mean_population<-0.0;


  float prey_birth_rate ;
  float predator_death_rate ;
  float predation_rate ;
  float predation_efficiency ;

  //pour 5000.0 0.005 pour 2000 0.0319; //pour 500m ??; //pour 1000m 0.144;    //0.00001  ;
  float micro_b;
  float micro_d;

  float danger_distance;
  float happiness_distance;


  int nb_preys_init ;
  int nb_predators_init ;




  float prey_to_born <- 0.0 ;
  float prey_to_die <- 0.0;
  float predator_to_die <- 0.0;
  float predator_to_born <- 0.0;

  string simulation_id;

  init {
    micro_b <- calibrate(int(predator_death_rate / predation_efficiency),int(prey_birth_rate /predation_rate),predation_rate,danger_distance);
    write "micro_b" +micro_b;
    micro_d <- calibrate(int(predator_death_rate / predation_efficiency),int(prey_birth_rate /predation_rate),predation_efficiency,happiness_distance);
    write "micro d" +micro_d;

    create prey number: nb_preys_init ;
    create predator number: nb_predators_init ;
  }

  int nb_preys <- nb_preys_init update: length (prey) ;
  int nb_predators <- nb_predators_init update: length (predator) ;

  reflex born_prey
  {
    prey_to_born <- prey_to_born + length(prey) * prey_birth_rate ;
    if prey_to_born >= 1.0 {
      create prey number: int(prey_to_born) ;
      prey_to_born <- prey_to_born - int(prey_to_born) ;
    }
  }


  /* reflex born_predator {
    predator_to_born <- predator_to_born + length(predator) * length(prey) *  predation_efficiency;
    if predator_to_born >= 1.0 {
      create predator number: int(predator_to_born) ;
      predator_to_born <- predator_to_born - int(predator_to_born) ;
    }
  }
  */
    reflex die_predator {
      predator_to_die <- predator_to_die + length (predator) * predator_death_rate;
      if predator_to_die >= 1.0 {
        ask int(predator_to_die) among predator {
          do die;
        }
        predator_to_die <- predator_to_die - int(predator_to_die) ;
      }
    }


  reflex updateMeans
  {
    prey_population <- prey_population + length(prey);
    mean_population <- float(mean(prey_population));
  }

  /*    reflex coucou when: (cycle mod(20) ) = 0
  {
    write "step " + cycle;
  }
  */
    reflex stopping when: cycle = 5000 or nb_preys = 0 or nb_predators = 0
  {
    do pause;
  }

  reflex saveFile when: cycle mod 5 = 0
  {
    save [cycle, currentSeed,length(prey), length(predator),  danger_distance,micro_b,happiness_distance,micro_d] to:"../outputs/output_"+simulation_id+".csv" type:"csv";  //_"+danger_distance+"_"+simulation_id+"
  }

}




species prey schedules: shuffle(prey) { //skills: [moving]
  float preys_speed ;
  bool is_in_danger <- false;
  int nb_predator_could_kill_me;
  aspect base {
    draw triangle(size_of_territory / 100) color: #lightblue ;
  }

  reflex basic_move {
    //do wander;
    location <- location +point({(rnd(500)-250)/10,(rnd(500)-250)/10});

    // self.location <- any_location_in(world.shape);

  }

  action isInDanger
  {
    nb_predator_could_kill_me <- length(predator at_distance danger_distance);
    if( nb_predator_could_kill_me >0)
    {
      is_in_danger <- true;

    }

  }

  reflex die_prey
  {
    nb_predator_could_kill_me <- length(predator at_distance danger_distance);
    if(flip(nb_predator_could_kill_me * micro_b))
    {
      do die;
    }

  }

}

species predator schedules: shuffle(predator) { //skills: [moving]
  float predators_speed ;
  int nbPreyArround <- length(prey at_distance(300)) update:length(prey at_distance(300));
  aspect base {
    draw square(size_of_territory / 100) color: #black ;
  }

  reflex basic_move {
    location <- location +point({(rnd(500)-250)/10,(rnd(500)-250)/10});
    //do wander;

  }

  reflex bornPredator
  {
    int preyneighbors <- length(prey at_distance happiness_distance);
    if(flip(preyneighbors * micro_d))
    {
      create predator number:1;
    }
  }
}

experiment prey_predator type: gui keep_seed:true {
  init
  {
    seed <- float(currentSeed);

  }
  parameter "Initial number of preys:" var: nb_preys_init <- 150 min: 1 category: "Population parameters" ;
  parameter "Initial number of predators:" var: nb_predators_init <- 50  min: 1 category: "Population parameters" ;

  parameter "Prey Birth Rate:" var: prey_birth_rate <- 0.05 min: 0.0 max: 1.0 category: "Macro parameters";
  parameter "Predator death rate:" var: predator_death_rate <- 0.03 min: 0.0 max: 1.0 category: "Macro parameters";
  parameter "Predation rate: " var: predation_rate <- 0.001  min: 0.0001 category: "Macro parameters";
  parameter "Predation efficiency: " var: predation_efficiency <- 0.0002  category: "Macro parameters";

  parameter "environment_size" var: size_of_territory <- 20000.0 #m min:1.0 #m category: "Micro parameters";
  parameter "danger_distance" var: danger_distance <- 5000.0 #m min:1.0 #m category: "Micro parameters";
  parameter "happiness_distance" var: happiness_distance <- 5000.0 #m min:1.0 #m category: "Micro parameters";

  parameter "simulation_id" var:simulation_id <- "0";
  parameter "currentSeed" var: currentSeed <- 131;

  output {
    monitor "mean_pop" value: mean_population;
    monitor "micro_b" value: micro_b;
    monitor "nb_predators" value: nb_predators;
    monitor "nb_preys" value: nb_preys;
  }
}

species type1 skills: [moving] {
  bool is_in_danger <- false;
  int nb_type2_could_kill_me;

  action basic_move {
    location <- any_location_in(world.shape);
  }

  action isInDanger
  {
    nb_type2_could_kill_me <- length(type2 at_distance predation_distance_cal);
    if( nb_type2_could_kill_me >0)
    {
      is_in_danger <- true;
    }
  }
}

species type2 skills: [moving] {
  action basic_move {
    location <- any_location_in(world.shape);
  }
}
