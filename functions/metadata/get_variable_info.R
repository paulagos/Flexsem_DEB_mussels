###############################################################################
# get_variable_info.R
#
# Returns plotting metadata for a DEB output variable.
#
# Output:
#   title  = Plot title
#   ylab   = Y-axis label
#   file   = Output filename
#   unit   = Variable unit (optional)
###############################################################################

get_variable_info <- function(variable = NULL){
  
  variables <- list(
    
    ###########################################################################
    # ENVIRONMENT
    ###########################################################################
    
    T = list(
      title = "Temperature",
      ylab  = expression(Temperature~(degree*C)),
      file  = "Temperature",
      unit  = "°C",
      type  = "environment",
      group = "Environment"
    ),
    
    S = list(
      title = "Salinity",
      ylab  = "Salinity (PSU)",
      file  = "Salinity",
      unit  = "PSU",
      type  = "environment",
      group = "Environment"
    ),
    
    CHL = list(
      title = "Chlorophyll-a",
      ylab  = expression(Chlorophyll~-a~"(mg m"^-3*")"),
      file  = "Chlorophyll_a",
      unit  = "mg m-3",
      type  = "environment",
      group = "Environment"
    ),
    
    FAECAL = list(
      title = "Faecal pellet production",
      ylab  = "Faecal pellets",
      file  = "Faecal_pellets",
      unit  = "",
      type  = "environment",
      group = "Environment"
    ),
    
    T_forcing = list(
      title = "Temperature forcing",
      ylab  = expression(Temperature~(degree*C)),
      file  = "Temperature_forcing",
      unit  = "°C",
      type  = "environment",
      group = "Environment"
    ),
    
    S_forcing = list(
      title = "Salinity forcing",
      ylab  = "Salinity (PSU)",
      file  = "Salinity_forcing",
      unit  = "PSU",
      type  = "environment",
      group = "Environment"
    ),
    
    CHL_forcing = list(
      title = "Chlorophyll forcing",
      ylab  = expression(Chlorophyll~-a~"(mg m"^-3*")"),
      file  = "Chlorophyll_forcing",
      unit  = "mg m-3",
      type  = "environment",
      group = "Environment"
    ),
    
    Tcorr = list(
      title = "Temperature correction factor",
      ylab  = "Correction factor",
      file  = "Temperature_correction",
      unit  = "",
      type  = "environment",
      group = "Environment"
    ),
    
    p_s = list(
      title = "Scaled functional response",
      ylab  = "Scaled functional response",
      file  = "Scaled_functional_response",
      unit  = "",
      type  = "environment",
      group = "Environment"
    ),
    
    ###########################################################################
    # DEB MUSSELS
    ###########################################################################
    
    ShellLength = list(
      title = "Shell length",
      ylab  = "Length (mm)",
      file  = "Shell_length",
      unit  = "mm",
      type  = "deb_mussels",
      group = "Growth"
    ),
    
    ShellWeight = list(
      title = "Shell weight",
      ylab  = "Weight (g)",
      file  = "Shell_weight",
      unit  = "g",
      type  = "deb_mussels",
      group = "Growth"
    ),
    
    TotalWeight = list(
      title = "Total weight",
      ylab  = "Weight (g)",
      file  = "Total_weight",
      unit  = "g",
      type  = "deb_mussels",
      group = "Growth"
    ),
    
    GSI = list(
      title = "Gonadosomatic Index",
      ylab  = "GSI (%)",
      file  = "GSI",
      unit  = "%",
      type  = "deb_mussels",
      group = "Reproduction"
    ),
  
  ###############################################################################
  # MUSSEL FARM
  ###############################################################################
  
  flowred = list(
    title = "Flow reduction factor",
    ylab  = "Flow reduction (-)",
    file  = "Flow_reduction",
    unit  = "-",
    type  = "mussel_farm",
    group = "Hydrodinamics"
  ),
  
  deadmus = list(
    title = "Mussel mortality",
    ylab  = "Number of dead mussels",
    file  = "Mussel_mortality",
    unit  = "# Ind.",
    type  = "mussels_farm",
    group = "Farm"
  ),
  
  pelmus = list(
    title = "Faecal pellet production",
    ylab  = "Pellet production",
    file  = "Pellet_production",
    unit  = "",
    type  = "mussels_farm",
    group = "Farm"
  ),
  
  nmus = list(
    title = "Mussels abundance",
    ylab  = expression("Mussels (" * m^-3 * ")"),
    file  = "Mussel_abundance",
    unit  = "individuals m-3",
    type  = "mussels_farm",
    group = "Farm"
  ),
  
  MusPop = list(
    title = "Farm mussel population",
    ylab  = expression("Mussel population (g DW " * m^-3 * ")"),
    file  = "Farm_biomass",
    unit  = "g DW m-3",
    type  = "mussels_farm",
    group = "Farm"
  ),
  
  MusPopN = list(
    title = "Normalized mussel population",
    ylab  = expression("Mussel population (mol N " * m^-3 * ")"),
    file  = "Mussel_population_N",
    unit  = "mol N m-3",
    type  = "mussels_farm",
    group = "Farm"
  ),
  
  nmusLoss = list(
    title = "Mussel losses",
    ylab  = "Number of mussels lost",
    file  = "Mussel_losses",
    unit  = "# Ind.",
    type  = "mussels_farm",
    group = "Farm"
  ),
  
  TotBio = list(
    title = "Total farm biomass",
    ylab  = expression("Total biomass (kg WW " * m^-3 * ")"),
    file  = "Total_biomass",
    unit  = "kg WW m-3",
    type  = "mussels_farm",
    group = "Farm"
  ),
  
  ###############################################################################
  # DEB STATE VARIABLES
  ###############################################################################
  
  dVdt = list(
    title = "Structural volume change",
    ylab  = expression(dV/dt),
    file  = "dVdt",
    unit  = "",
    type  = "state_variables",
    group = "Growth"
  ),
  
  dEdt = list(
    title = "Reserve energy change",
    ylab  = expression(dE/dt),
    file  = "dEdt",
    unit  = "",
    type  = "state_variables",
    group = "Energy"
  ),
  
  dE_Rdt = list(
    title = "Reproduction buffer change",
    ylab  = expression(dE[R]/dt),
    file  = "dE_Rdt",
    unit  = "",
    type  = "state_variables",
    group = "Reproduction"
  ),
  
  volmus = list(
    title = "Structural volume",
    ylab  = "Structural volume",
    file  = "Structural_volume",
    unit  = "",
    type  = "state_variables",
    group = "Growth"
  ),
  
  volmusSL = list(
    title = "Structural volume for SL",
    ylab  = "Structural volume for SL",
    file  = "Structural_volume for SL",
    unit  = "",
    type  = "state_variables",
    group = "Growth"
  ),
  
  resmus = list(
    title = "Reserve energy",
    ylab  = "Reserve energy",
    file  = "Reserve_energy",
    unit  = "",
    type  = "state_variables",
    group = "Energy"
  ),
  
  repmus = list(
    title = "Reproduction buffer",
    ylab  = "Reproduction buffer",
    file  = "Reproduction_buffer",
    unit  = "",
    type  = "state_variables",
    group = "Reproduction"
  ),
  
  ###############################################################################
  # DEB MUSSELS VARIABLES
  ###############################################################################
  
  Ing_XmT = list(
    title = "Ingestion rate",
    ylab  = "Ingestion rate",
    file  = "Ingestion_rate",
    unit  = "",
    type  = "deb_mussels",
    group = "Feeding"
  ),
  
  p_XmT = list(
    title = "Maximum ingestion flux",
    ylab  = "Energy flux",
    file  = "Maximum_ingestion_flux",
    unit  = "",
    type  = "deb_mussels",
    group = "Feeding"
  ),
  
  p_mT = list(
    title = "Somatic maintenance",
    ylab  = "Energy flux",
    file  = "Somatic_maintenance",
    unit  = "Physiology",
    type  = "deb_mussels",
    group = "Physiology"
  ),
  
  p_AmT = list(
    title = "Maximum assimilation flux",
    ylab  = "Energy flux",
    file  = "Maximum_assimilation_flux",
    unit  = "",
    type  = "deb_mussels",
    group = "Physiology"
  ),
  
  SLV = list(
    title = "Structural shell length",
    ylab  = "Length (cm)",
    file  = "Structural_shell_length",
    unit  = "cm",
    type  = "deb_mussels",
    group = "Growth"
  ),
  
  SL = list(
    title = "Shell length",
    ylab  = "Length (cm)",
    file  = "Shell_length",
    unit  = "cm",
    type  = "deb_mussels",
    group = "Growth"
  ),
  
  FDW = list(
    title = "Flesh dry weight",
    ylab  = "Dry weight (g)",
    file  = "Flesh_dry_weight",
    unit  = "g",
    type  = "deb_mussels",
    group = "Growth"
  ),
  
  GDW = list(
    title = "Total dry weight",
    ylab  = "Dry weight (g)",
    file  = "Total_dry_weight",
    unit  = "g",
    type  = "deb_mussels",
    group = "Growth"
  ),
  
  Eratio = list(
    title = "Reserve density",
    ylab  = "Reserve density (-)",
    file  = "Reserve_density",
    unit  = "-",
    type  = "deb_mussels",
    group = "Physiology"
  ),
  
  f = list(
    title = "Scaled functional response",
    ylab  = "Scaled functional response (-)",
    file  = "Scaled_functional_response",
    unit  = "-",
    type  = "deb_mussels",
    group = "Physiology"
  ),
  
  Ed = list(
    title = "Reserve energy density",
    ylab  = "Reserve energy",
    file  = "Reserve_energy_density",
    unit  = "",
    type  = "deb_mussels",
    group = "Physiology"
  ),
  
  Maint_C = list(
    title = "Maintenance costs",
    ylab  = "Carbon flux",
    file  = "Maintenance_costs",
    unit  = "",
    type  = "deb_mussels",
    group = "Physiology"
  ),
  
  shellV = list(
    title = "Shell volume",
    ylab  = expression("Volume ("*cm^3*")"),
    file  = "Shell_volume",
    unit  = "cm3",
    type  = "deb_mussels",
    group = "Shell"
  ),
  
  shellC = list(
    title = "Shell carbon",
    ylab  = "Carbon (g)",
    file  = "Shell_carbon",
    unit  = "g",
    type  = "deb_mussels",
    group = "Shell"
  ),
  
  shellCa = list(
    title = "Shell calcium",
    ylab  = "Calcium (g)",
    file  = "Shell_calcium",
    unit  = "g",
    type  = "deb_mussels",
    group = "Shell"
  ),
  
  spawmus = list(
    title = "Spawning event",
    ylab  = "Spawn",
    file  = "Spawning",
    unit  = "",
    type  = "deb_mussels",
    group = "Reproduction"
  ),
  
  nGSI = list(
    title = "Normalized GSI",
    ylab  = "Normalized GSI",
    file  = "Normalized_GSI",
    unit  = "",
    type  = "deb_mussels",
    group = "Reproduction"
  ),
  
  ###########################################################################
  # PHYSIOLOGICAL STRESS AND SURVIVAL
  ###########################################################################
  
  AcuteStress = list(
    title = "Acute physiological stress",
    ylab  = "Maintenance deficit ratio (-)",
    file  = "Acute_stress",
    unit  = "-",
    type  = "deb_mussels",
    group = "Physiological Stress"
  ),
  
  Damage = list(
    title = "Accumulated structural damage",
    ylab  = "Structural damage (-)",
    file  = "Structural_damage",
    unit  = "-",
    type  = "deb_mussels",
    group = "Physiological Stress"
  ),
  
  StressIndex = list(
    title = "Physiological stress index",
    ylab  = "Stress index (-)",
    file  = "Stress_index",
    unit  = "-",
    type  = "deb_mussels",
    group = "Physiological Stress"
  ),
  
  survival = list(
    title = "Physiological survival index",
    ylab  = "Survival index (-)",
    file  = "Survival_index",
    unit  = "-",
    type  = "deb_mussels",
    group = "Mortality"
  ),
  
  ###############################################################################
  # DEB FLUXES
  ###############################################################################
  
  flow_P_A = list(
    title = "Assimilation flux",
    ylab  = "Flux_P_A",
    file  = "Assimilation_flux",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  flow_P_G = list(
    title = "Growth flux",
    ylab  = "Flux P_G",
    file  = "Growth_flux",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  flow_P_C = list(
    title = "Mobilization flux",
    ylab  = "Flux P_C",
    file  = "Mobilization_flux",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  flow_P_M = list(
    title = "Somatic maintenance flux",
    ylab  = "Flux P_M",
    file  = "Somatic_maintenance_flux",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  flow_P_J = list(
    title = "Maturity maintenance flux",
    ylab  = "Flux P_J",
    file  = "Maturity_maintenance_flux",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  flow_P_R = list(
    title = "Reproduction flux",
    ylab  = "Flux P_R",
    file  = "Reproduction_flux",
    unit  = "",
    type  = "deb_fluxes",
    group = "Reproduction"
  ),
  
  flow_P_D = list(
    title = "Dissipation flux",
    ylab  = "Flux P_D",
    file  = "Dissipation_flux",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  shrink_P_M = list(
    title = "Maintenance stress",
    ylab  = "Shrink_P_M",
    file  = "Maintenance_stress",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  shrink_P_J = list(
    title = "Maturity maintenance stress",
    ylab  = "Shrink_P_J",
    file  = "Maturity_maintenance_stress",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  shrink_E_R = list(
    title = "Reproduction buffer stress",
    ylab  = "Shrink_E_R",
    file  = "Reproduction_buffer_stress",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  shrink_V = list(
    title = "Structural shrinkage",
    ylab  = "Shrink_V",
    file  = "Structural_shrinkage",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  shrink = list(
    title = "Shrinkage indicator",
    ylab  = "Shrink",
    file  = "Shrinkage",
    unit  = "",
    type  = "deb_fluxes",
    group = "Energy"
  ),
  
  ###########################################################################
  # MAINTENANCE BALANCE DIAGNOSTICS
  ###########################################################################
  
  SomaticSupply = list(
    title = "Somatic energy supply",
    ylab  = "Energy flux",
    file  = "Somatic_energy_supply",
    unit  = "",
    type  = "deb_fluxes",
    group = "Maintenance Balance"
  ),
  
  MaintenanceDemand = list(
    title = "Somatic maintenance demand",
    ylab  = "Energy flux",
    file  = "Maintenance_demand",
    unit  = "",
    type  = "deb_fluxes",
    group = "Maintenance Balance"
  ),
  
  MaintenanceDeficit = list(
    title = "Somatic maintenance deficit",
    ylab  = "Energy flux",
    file  = "Maintenance_deficit",
    unit  = "",
    type  = "deb_fluxes",
    group = "Maintenance Balance"
  ),
  
  ###########################################################################
  # REPRODUCTIVE BUFFER DIAGNOSTICS
  ###########################################################################
  
  ReproInput = list(
    title = "Reproductive buffer input",
    ylab  = "Energy flux",
    file  = "Reproductive_buffer_input",
    unit  = "",
    type  = "deb_fluxes",
    group = "Reproduction"
  ),
  
  ReproLoss = list(
    title = "Reproductive buffer loss",
    ylab  = "Energy flux",
    file  = "Reproductive_buffer_loss",
    unit  = "",
    type  = "deb_fluxes",
    group = "Reproduction"
  ),
  
  ReproBalance = list(
    title = "Reproductive buffer balance",
    ylab  = "Net energy flux",
    file  = "Reproductive_buffer_balance",
    unit  = "",
    type  = "deb_fluxes",
    group = "Reproduction"
  ),
  
  ###############################################################################
  # SEDIMENT
  ###############################################################################
  
  Sed_Faecal = list(
    title = "Accumulated faecal pellets",
    ylab  = expression("Faecal pellets (g DW "*m^-2*")"),
    file  = "Accumulated_faecal_pellets",
    unit  = "g DW m-2",
    type  = "sediment",
    group = "Sediment"
  ),
  
  flux_FAECAL = list(
    title = "Faecal pellet deposition flux",
    ylab  = expression("Flux (g DW "*m^-2*" "*d^-1*")"),
    file  = "Faecal_pellet_flux",
    unit  = "g DW m-2 d-1",
    type  = "sediment",
    group = "Sediment"
  )
  
)
  
  if(is.null(variable)){
    return(variables)
  }
  
  if(!variable %in% names(variables)){
    
    stop(
      paste(
        "Variable",
        variable,
        "has not yet been defined in get_variable_info()."
      )
    )
    
  }
  
  return(variables[[variable]])
  
}