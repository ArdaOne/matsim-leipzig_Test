package org.matsim.run.prepare;

import cadyts.utilities.misc.DynamicData;
import ch.sbb.matsim.config.SwissRailRaptorConfigGroup;
import ch.sbb.matsim.routing.pt.raptor.RaptorIntermodalAccessEgress;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Network;
import org.matsim.application.options.ShpOptions;
import org.matsim.contrib.drt.analysis.zonal.DrtZonalSystemParams;
import org.matsim.contrib.drt.fare.DrtFareParams;
import org.matsim.contrib.drt.optimizer.insertion.extensive.ExtensiveInsertionSearchParams;
import org.matsim.contrib.drt.optimizer.rebalancing.RebalancingParams;
import org.matsim.contrib.drt.optimizer.rebalancing.mincostflow.MinCostFlowRebalancingStrategyParams;
import org.matsim.contrib.drt.routing.DrtRoute;
import org.matsim.contrib.drt.routing.DrtRouteFactory;
import org.matsim.contrib.drt.run.*;
import org.matsim.contrib.drt.speedup.DrtSpeedUpParams;
import org.matsim.contrib.dvrp.run.AbstractDvrpModeModule;
import org.matsim.contrib.dvrp.run.DvrpConfigGroup;
import org.matsim.contrib.dvrp.run.DvrpModule;
import org.matsim.contrib.dvrp.run.DvrpQSimComponents;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.config.groups.ChangeModeConfigGroup;
import org.matsim.core.config.groups.PlanCalcScoreConfigGroup;
import org.matsim.core.config.groups.SubtourModeChoiceConfigGroup;
import org.matsim.core.controler.AbstractModule;
import org.matsim.core.controler.Controler;
import org.matsim.core.controler.OutputDirectoryHierarchy;
import org.matsim.core.controler.events.StartupEvent;
import org.matsim.core.controler.listener.StartupListener;
import org.matsim.core.network.NetworkUtils;
import org.matsim.core.network.algorithms.TransportModeNetworkFilter;
import org.matsim.core.utils.io.IOUtils;
import org.matsim.extensions.pt.fare.intermodalTripFareCompensator.IntermodalTripFareCompensatorConfigGroup;
import org.matsim.extensions.pt.fare.intermodalTripFareCompensator.IntermodalTripFareCompensatorsConfigGroup;
import org.matsim.extensions.pt.fare.intermodalTripFareCompensator.IntermodalTripFareCompensatorsModule;
import org.matsim.extensions.pt.routing.EnhancedRaptorIntermodalAccessEgress;
import org.matsim.extensions.pt.routing.ptRoutingModes.PtIntermodalRoutingModesConfigGroup;
import org.matsim.extensions.pt.routing.ptRoutingModes.PtIntermodalRoutingModesModule;
import org.matsim.run.LeipzigPtFareModule;
import org.opengis.feature.simple.SimpleFeature;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * This class configures drt config, stops, transitStopsFile and vehicles regarding enum DrtCase of RunLeipzigScenario.
 */
public final class DrtCaseSetup {

	private static final Logger log = LogManager.getLogger(DrtCaseSetup.class);
	/*private static final ShpOptions flexaArea2021 = new ShpOptions(Path.of(
			"input/v1.2/drtServiceArea/leipzig_flexa_service_area_2021.shp"),
			null, null); */

	private static final String errorMessage = "Unexpected value: ";

	//this is not nice because the static set is only filled in prepareConfig
	static Set<String> drtModes = new HashSet<>();

	/**
	 * Defines if drt is modelled at all (none), with 2 separate modes (twoSeparateServiceAreas) or with 1 single drt mode (oneServiceArea).
	 * As this class is only triggered if a shp of the drt service area was provided, none is inactive for now
	 * <br> <b>tschlenther, feb' 02, 2024:</b> all the necessary information (nrOfServices, nrOfVehicles, modes etc.) shall be provided by the shape file, so this enum is strictly not needed.
	 */
	public enum DrtCase {/*none,*/ twoSeparateServiceAreas /*, oneServiceArea*/}

	/**
	 * Defines if intermodality between drt and pt is modelled or not.
	 */
	public enum PtDrtIntermodality {drtAndPtSeparateFromEachOther, drtAsAccessEgressForPt}

	public DrtCaseSetup(){	}

	/**
	 * prepare config for drt simulation.
	 */
	public static void prepareConfig(Config config,/* DrtCase drtCase,*/ ShpOptions drtAreas) throws URISyntaxException {

		MultiModeDrtConfigGroup multiModeDrtConfigGroup = ConfigUtils.addOrGetModule(config, MultiModeDrtConfigGroup.class);
		DvrpConfigGroup dvrpConfigGroup = ConfigUtils.addOrGetModule(config, DvrpConfigGroup.class);

		LeipzigPtFareModule ptFareModule = new LeipzigPtFareModule();

		//set fare params; flexa has the same prices as leipzig PT: Values taken out of LeipzigPtFareModule -sm0522
		Double ptBaseFare = ptFareModule.getNormalPtBaseFare();
		Double ptDistanceFare = ptFareModule.getNormalDistanceBasedFare();

		DrtFareParams drtFareParams = new DrtFareParams();
		drtFareParams.baseFare = ptBaseFare;
		drtFareParams.distanceFare_m = ptDistanceFare;
		drtFareParams.timeFare_h = 0.;
		drtFareParams.dailySubscriptionFee = 0.;


		log.info("reading " + drtAreas.getShapeFile().toString());
		for (SimpleFeature feature : drtAreas.readFeatures()) {
//					String name = (String) feature.getAttribute("Name");
			String drtMode = String.valueOf(feature.getAttribute("mode"));
			if (drtMode.equals("null")) {
				throw new IllegalArgumentException("could not find 'mode' attribute in the given shape file at " + drtAreas.getShapeFile().toString());
			} else {
				drtModes.add(drtMode);
			}
		}



		if (multiModeDrtConfigGroup.getModalElements().isEmpty()) {
			for (String mode : drtModes){
				createDrtModeConfigGroup(multiModeDrtConfigGroup, mode);
			}
//			createDrtModeConfigGroup(multiModeDrtConfigGroup, TransportMode.drt + "North", drtArea.getShapeFile().toString());
//			createDrtModeConfigGroup(multiModeDrtConfigGroup, TransportMode.drt + "Southeast", drtArea.getShapeFile().toString());
		}

		multiModeDrtConfigGroup.getModalElements().forEach(drtConfigGroup -> {
			drtConfigGroup.addParameterSet(drtFareParams);
			DrtConfigs.adjustDrtConfig(drtConfigGroup, config.planCalcScore(), config.plansCalcRoute());
//			drtModes.add(drtConfigGroup.getMode());

			configureNecessaryConfigGroups(config, drtConfigGroup.getMode());
			DrtSpeedUpParams drtSpeedUpParams = new DrtSpeedUpParams();
			drtSpeedUpParams.fractionOfIterationsSwitchOn = 0.0;
			drtSpeedUpParams.fractionOfIterationsSwitchOff = 1.0;
			drtSpeedUpParams.firstSimulatedDrtIterationToReplaceInitialDrtPerformanceParams = 10000;
			// update to real world data
			drtSpeedUpParams.initialWaitingTime = 345.6;
			// update to real world data
			drtSpeedUpParams.initialInVehicleBeelineSpeed = 20/3.6;
			drtSpeedUpParams.intervalDetailedIteration = 10000;
			drtSpeedUpParams.waitingTimeUpdateDuringSpeedUp = DrtSpeedUpParams.WaitingTimeUpdateDuringSpeedUp.Disabled;
			drtConfigGroup.addParameterSet(drtSpeedUpParams);
		});

//		switch (drtCase) {
//			case twoSeparateServiceAreas -> {
//				if (multiModeDrtConfigGroup.getModalElements().isEmpty()) {
//					createDrtModeConfigGroup(multiModeDrtConfigGroup, TransportMode.drt + "North", drtArea.getShapeFile().toString());
//					createDrtModeConfigGroup(multiModeDrtConfigGroup, TransportMode.drt + "Southeast", drtArea.getShapeFile().toString());
//				}
//				//flexa case with 2 separate drt bubbles (north and southeast) -> 2 separate drt modes
//
//				multiModeDrtConfigGroup.getModalElements().forEach(drtConfigGroup -> {
//					drtConfigGroup.addParameterSet(drtFareParams);
//					DrtConfigs.adjustDrtConfig(drtConfigGroup, config.planCalcScore(), config.plansCalcRoute());
//					drtModes.add(drtConfigGroup.getMode());
//
//
//					configureNecessaryConfigGroups(config, drtConfigGroup.getMode());
//					DrtSpeedUpParams drtSpeedUpParams = new DrtSpeedUpParams();
//					drtSpeedUpParams.fractionOfIterationsSwitchOn = 0.0;
//					drtSpeedUpParams.fractionOfIterationsSwitchOff = 1.0;
//					drtSpeedUpParams.firstSimulatedDrtIterationToReplaceInitialDrtPerformanceParams = 10000;
//					// update to real world data
//					drtSpeedUpParams.initialWaitingTime = 345.6;
//					// update to real world data
//					drtSpeedUpParams.initialInVehicleBeelineSpeed = 20/3.6;
//					drtSpeedUpParams.intervalDetailedIteration = 10000;
//					drtSpeedUpParams.waitingTimeUpdateDuringSpeedUp = DrtSpeedUpParams.WaitingTimeUpdateDuringSpeedUp.Disabled;
//					drtConfigGroup.addParameterSet(drtSpeedUpParams);
//				});
//
//
//			}
//
//			case oneServiceArea -> {
//				//"normal" drt, modelled as one single drt mode
//				if (multiModeDrtConfigGroup.getModalElements().isEmpty()) {
//					createDrtModeConfigGroup(multiModeDrtConfigGroup, TransportMode.drt, drtArea.getShapeFile().toString());
//				}
//
//				multiModeDrtConfigGroup.getModalElements().forEach(drtConfigGroup -> {
//					drtConfigGroup.addParameterSet(drtFareParams);
//					DrtConfigs.adjustDrtConfig(drtConfigGroup, config.planCalcScore(), config.plansCalcRoute());
//					drtModes.add(drtConfigGroup.getMode());
//
//					configureNecessaryConfigGroups(config, drtConfigGroup.getMode());
//					DrtSpeedUpParams drtSpeedUpParams = new DrtSpeedUpParams();
//					drtSpeedUpParams.fractionOfIterationsSwitchOn = 0.0;
//					drtSpeedUpParams.fractionOfIterationsSwitchOff = 1.0;
//					drtSpeedUpParams.firstSimulatedDrtIterationToReplaceInitialDrtPerformanceParams = 10000;
//					// update to real world data
//					drtSpeedUpParams.initialWaitingTime = 345.6;
//					// update to real world data
//					drtSpeedUpParams.initialInVehicleBeelineSpeed = 20/3.6;
//					drtSpeedUpParams.intervalDetailedIteration = 10000;
//					drtSpeedUpParams.waitingTimeUpdateDuringSpeedUp = DrtSpeedUpParams.WaitingTimeUpdateDuringSpeedUp.Disabled;
//					drtConfigGroup.addParameterSet(drtSpeedUpParams);
//
//				});
//			}
//			default -> throw new IllegalStateException(errorMessage + (drtCase));
//		}

		//drt modes have to be set as network modes in dvrp CfgGroup
		dvrpConfigGroup.networkModes = drtModes;
		//after adding mode specific multiModeDrtParams -> adjust
	}

	/**
	 * prepare scenario for drt simulation. more specifically, create input vehicles and stops files.
	 * this method does not (!?) change the network. For this, please refer to NetworkOptions.prepareDRT
	 */
	public static void prepareScenario(Scenario scenario,/* DrtCase drtCase, */ ShpOptions drtAreas, String version) {

		scenario.getPopulation().getFactory().getRouteFactories().setRouteFactory(DrtRoute.class, new DrtRouteFactory());
		// (matsim core does not know about DRT routes. This makes it possible to read them before the controler is there.)



		CreateDrtStopsFromNetwork drtStopsCreator = new CreateDrtStopsFromNetwork();
		MultiModeDrtConfigGroup multiModeDrtConfigGroup = ConfigUtils.addOrGetModule(scenario.getConfig(), MultiModeDrtConfigGroup.class);

//		String drtMode;
//		switch (drtCase) {
//			case twoSeparateServiceAreas -> {
				//flexa case with 2 separate drt bubbles (north and southeast) -> 2 separate drt modes

		log.info("reading " + drtAreas.getShapeFile().toString());
				for (SimpleFeature feature : drtAreas.readFeatures()) {
//				for (String drtMode : this.drtModes2Feature.keySet()) {
//					String name = (String) feature.getAttribute("Name");
					String drtMode = String.valueOf(feature.getAttribute("mode"));
					if (drtMode.equals("null")) {
						throw new IllegalArgumentException("could not find 'mode' attribute in the given shape file at " + drtAreas.getShapeFile().toString());
					}
					Integer noVehicles = (Integer) feature.getAttribute("noVehicles");
					if (noVehicles.equals(null)){
						throw new IllegalArgumentException("could not find 'noVehicles' attribute in the given shape file at " + drtAreas.getShapeFile().toString());
					}

					log.info("filtering network for mode " + drtMode + ". Before, the number of links equals " + scenario.getNetwork().getLinks().size());
					Network filteredNetwork = NetworkUtils.createNetwork();
					TransportModeNetworkFilter filter = new TransportModeNetworkFilter(scenario.getNetwork());
					filter.filter(filteredNetwork, Sets.newHashSet(drtMode));
					log.info("filtered network contains " + filteredNetwork.getLinks().size() + " links");

					log.info("attempting to create " + noVehicles + " drt vehicles for mode " + drtMode);
					new LeipzigDrtVehicleCreator().createDrtVehiclesForSingleArea(scenario.getVehicles(), filteredNetwork,
							feature, noVehicles, drtMode);

					//normally the following code would be set in prepareConfig, but..
					//.. the stops creator relies on a network with drt modes. Drt modes are added in RunLeipzigScenario.prepareScenario()..
					//.. so stops are created after that step -sme0823
//					multiModeDrtConfigGroup.getModalElements().forEach(drtConfigGroup -> {
					DrtConfigGroup drtConfigGroup = multiModeDrtConfigGroup.getModalElements().stream().
						filter(cfg -> cfg.getMode().equals(drtMode))
						.findFirst().orElseThrow();
						{
							//path, tho which stops.xml is saved
							URL path = IOUtils.extendUrl(scenario.getConfig().getContext(), "leipzig-v" + version + "-" + drtMode + "-stops.xml");
	//						URL path = IOUtils.extendUrl(scenario.getConfig().getContext(), "leipzig-v" + version + "-" + drtConfigGroup.getMode() + "-area.shp");
							File file = null;
							try {
								file = new File(path.toURI());
							} catch (URISyntaxException e) {
								log.fatal(e);
							}


							//create drt stops and save them next to config -> put it as input stops file.
							//unfortunately there is no scenario.setDrtStops, so we have to do this workaround. -sme0723
							drtStopsCreator.processNetworkForStopCreation(scenario.getNetwork(), true, (Geometry) feature.getDefaultGeometry(),
								drtAreas.getShapeFile().toString() + "_" + drtConfigGroup.getMode() + "_stops.csv", drtConfigGroup.getMode(),
								file.toString(), drtAreas);

							//naming pattern comes from @DrtStopsWriter line 81. Should be ok to hard code it here. -sme0523
							drtConfigGroup.transitStopFile = file.toString();

	//						new ShapeFileWriter().writeGeometries(List.of(feature), file.toString());
	//						drtConfigGroup.drtServiceAreaShapeFile = file.toString();
	//						drtConfigGroup.operationalScheme = DrtConfigGroup.OperationalScheme.serviceAreaBased; //TODO move this to prepareConfig

//							configureNecessaryConfigGroups(scenario.getConfig(), drtConfigGroup.getMode());
						}
//					});
				}
//			}

//			case oneServiceArea -> {
//				//"normal" drt, modelled as one single drt mode
//				drtMode = TransportMode.drt;
//
//				//make the 400 configurable??? -sme0723
//				new LeipzigDrtVehicleCreator().createDrtVehicles(scenario.getVehicles(), scenario.getNetwork(),
//						drtArea, 400, drtMode);
//
//				//normally the following code would be set in prepareConfig, but..
//				//.. the stops creator relies on a network with drt modes. Drt modes are added in RunLeipzigScenario.prepareScenario()..
//				//.. so stops are created after that step -sme0823
//				multiModeDrtConfigGroup.getModalElements().forEach(drtConfigGroup -> {
//
//					//path, tho which stops.xml is saved
//					URL path = IOUtils.extendUrl(scenario.getConfig().getContext(), "leipzig-v" + version + "-" + drtConfigGroup.getMode() + "-stops.xml");
//					File stopsFile = null;
//					try {
//						stopsFile = new File(path.toURI());
//					} catch (URISyntaxException e) {
//						log.fatal(e);
//					}
//
//					//create drt stops and save them next to config -> put it as input stops file.
//					//unfortunately there is no scenario.setDrtStops, so we have to do this workaround. -sme0723
//					drtStopsCreator.processNetworkForStopCreation(scenario.getNetwork(), true, drtArea.getGeometry(),
//							drtArea.getShapeFile().toString() + "_" + drtConfigGroup.getMode() + "_stops.csv", drtConfigGroup.getMode(),
//							stopsFile.toString(), drtArea);
//
//					//naming pattern comes from @DrtStopsWriter line 81. Should be ok to hard code it here. -sme0523
//					drtConfigGroup.transitStopFile = stopsFile.toString();
//
//				});
//
//			}
//			default -> throw new IllegalStateException(errorMessage + (drtCase));
//		}
	}

	/**
	 * prepare controler for drt simulation.
	 */
	public static void prepareControler(Controler controler,/* DrtCase drtCase,*/ ShpOptions drtAreas, PtDrtIntermodality ptDrtIntermodality) {

		MultiModeDrtConfigGroup multiModeDrtConfigGroup = ConfigUtils.addOrGetModule(controler.getConfig(), MultiModeDrtConfigGroup.class);
		controler.addOverridingModule(new DvrpModule());
		controler.addOverridingModule(new MultiModeDrtModule());
		controler.configureQSimComponents(DvrpQSimComponents.activateAllModes(multiModeDrtConfigGroup));

		// if drt is stopBased, we want to write the drt stops into the global output -sme0723
		for (DrtConfigGroup drtCfg : multiModeDrtConfigGroup.getModalElements()) {
			if (drtCfg.operationalScheme.equals(DrtConfigGroup.OperationalScheme.stopbased)) {
				controler.addOverridingModule(new AbstractDvrpModeModule(drtCfg.getMode()) {
					@Override
					public void install() {
						bindModal(DrtCaseSetup.StopsControlerListener.class).toProvider(modalProvider(
								getter -> new DrtCaseSetup.StopsControlerListener(drtCfg.getMode(),
										getter.get(OutputDirectoryHierarchy.class), drtCfg.transitStopFile
								)));
						addControlerListenerBinding().to(modalKey(DrtCaseSetup.StopsControlerListener.class));
					}
				});
			}
		}

//		switch (drtCase) {
//			case twoSeparateServiceAreas -> {
				//flexa case with 2 separate drt bubbles (north and southeast) -> 2 separate drt modes

				//if intermodality between pt and drt -> only railways are tagged as intermodal stations (this is how it is handled in reality) -sme0723
				if (ptDrtIntermodality.equals(PtDrtIntermodality.drtAsAccessEgressForPt)) {
					preparePtDrtIntermodality(controler, drtAreas, true);
				}
//			}
//
//			case oneServiceArea -> {
//				//"normal" drt, modelled as one single drt mode
//
//				if (ptDrtIntermodality.equals(PtDrtIntermodality.drtAsAccessEgressForPt)) {
//					preparePtDrtIntermodality(controler, drtArea, false);
//				}
//			}
//			default -> throw new IllegalStateException(errorMessage + (drtCase));
//		}
	}

	/**
	 * if no modal params existing, we have to create them.
	 */
	private static void createDrtModeConfigGroup(MultiModeDrtConfigGroup multiModeDrtConfigGroup, String mode/*, String pathToShp*/) {
		DrtConfigGroup drtConfigGroup = new DrtConfigGroup();
		drtConfigGroup.mode = mode;
		drtConfigGroup.operationalScheme = DrtConfigGroup.OperationalScheme.stopbased;
		drtConfigGroup.maxTravelTimeAlpha = 1.5;
		drtConfigGroup.maxTravelTimeBeta = 1200.;
		drtConfigGroup.maxWaitTime = 1200.;
		drtConfigGroup.maxWalkDistance = 1500.;
		drtConfigGroup.rejectRequestIfMaxWaitOrTravelTimeViolated = false;
		drtConfigGroup.stopDuration = 60.;
		drtConfigGroup.useModeFilteredSubnetwork = true;

		//add insertionSearch params
		ExtensiveInsertionSearchParams insertionSearchParams = new ExtensiveInsertionSearchParams();
		drtConfigGroup.addParameterSet(insertionSearchParams);

		//we do not simulate drt explicitly anymore, so rebalancing is commented out. tschlenther feb' 24
		{
//			//add rebalancing params and configure standard values
//			RebalancingParams rebalancingParams = new RebalancingParams();
//
//		MinCostFlowRebalancingStrategyParams rebalancingStrategyParams = new MinCostFlowRebalancingStrategyParams();
//		rebalancingStrategyParams.targetAlpha = 0.5;
//		rebalancingStrategyParams.targetBeta = 0.5;
//		rebalancingParams.addParameterSet(rebalancingStrategyParams);
//		drtConfigGroup.addParameterSet(rebalancingParams);
//
//		DrtZonalSystemParams zonalSystemParams = new DrtZonalSystemParams();
//		zonalSystemParams.zonesGeneration = DrtZonalSystemParams.ZoneGeneration.ShapeFile;
//		zonalSystemParams.zonesShapeFile = new File(pathToShp).getAbsolutePath(); //this needs to be adjusted: should only be the one corresponding feature and not the whole shape file!!
//		drtConfigGroup.addParameterSet(zonalSystemParams);
//
		}

		multiModeDrtConfigGroup.addParameterSet(drtConfigGroup);
	}

	/**
	 * configure all other config groups, where drt modes need to be included: SMC, ChangeMode, ModeParams.
	 */
	private static void configureNecessaryConfigGroups(Config config, String mode) {

		PlanCalcScoreConfigGroup planCalcScoreConfigGroup = ConfigUtils.addOrGetModule(config, PlanCalcScoreConfigGroup.class);
		ChangeModeConfigGroup changeModeConfigGroup = ConfigUtils.addOrGetModule(config, ChangeModeConfigGroup.class);
		SubtourModeChoiceConfigGroup smcCfg = ConfigUtils.addOrGetModule(config, SubtourModeChoiceConfigGroup.class);

		//add drt mode to modeParams if it does not exist yet
		if (!planCalcScoreConfigGroup.getModes().containsKey(mode)) {
			PlanCalcScoreConfigGroup.ModeParams modeParams = new PlanCalcScoreConfigGroup.ModeParams(mode);
			modeParams.setConstant(planCalcScoreConfigGroup.getModes().get(TransportMode.pt).getConstant());
			modeParams.setMarginalUtilityOfTraveling(0.);

			PlanCalcScoreConfigGroup.ScoringParameterSet scoringParams = planCalcScoreConfigGroup.getOrCreateScoringParameters(null);
			scoringParams.addModeParams(modeParams);
		}

		//add drt modes to changeModeCfgGroup
		if (!Arrays.stream(changeModeConfigGroup.getModes()).toList().contains(mode)) {
			List<String> modes = new ArrayList<>(Arrays.asList(changeModeConfigGroup.getModes()));
			modes.add(mode);
			changeModeConfigGroup.setModes(modes.toArray(new String[modes.size()]));
		}

		//add drt modes to SMC
		if (!Arrays.stream(smcCfg.getModes()).toList().contains(mode)) {
			List<String> modes = new ArrayList<>(Arrays.asList(smcCfg.getModes()));
			modes.add(mode);
			smcCfg.setModes(modes.toArray(new String[modes.size()]));
		}
	}

	private static void preparePtDrtIntermodality(Controler controler, ShpOptions shp, boolean railwaysOnly) {

		new PrepareTransitSchedule().prepareDrtIntermodality(controler.getScenario().getTransitSchedule(), shp, railwaysOnly);

		ConfigUtils.addOrGetModule(controler.getConfig(), MultiModeDrtConfigGroup.class).getModalElements().stream().findFirst().ifPresent(drtConfigGroup ->
				drtConfigGroup.getDrtFareParams().ifPresent(drtFareParams ->
						prepareDrtFareCompensation(controler, drtModes, drtFareParams.baseFare))); //this only works if prepareConfig was called with the same ShpOptions
	}

	private static void prepareDrtFareCompensation(Controler controler, Set<String> nonPtModes, Double ptBaseFare) {
		IntermodalTripFareCompensatorsConfigGroup intermodalTripFareCompensatorsConfigGroup =
				ConfigUtils.addOrGetModule(controler.getConfig(), IntermodalTripFareCompensatorsConfigGroup.class);

		IntermodalTripFareCompensatorConfigGroup drtFareCompensator = new IntermodalTripFareCompensatorConfigGroup();
		drtFareCompensator.setCompensationCondition(IntermodalTripFareCompensatorConfigGroup.CompensationCondition.PtModeUsedAnywhereInTheDay);

		//Flexa is integrated into pt system, so users only pay once
		drtFareCompensator.setCompensationMoneyPerTrip(ptBaseFare);
		drtFareCompensator.setNonPtModes(ImmutableSet.copyOf(nonPtModes));

		intermodalTripFareCompensatorsConfigGroup.addParameterSet(drtFareCompensator);
		controler.addOverridingModule(new IntermodalTripFareCompensatorsModule());

		//for intermodality between pt and drt the following modules have to be installed and configured
		String artificialPtMode = "pt_w_drt_allowed";
		PtIntermodalRoutingModesConfigGroup ptIntermodalRoutingModesConfig = ConfigUtils.addOrGetModule(controler.getConfig(), PtIntermodalRoutingModesConfigGroup.class);
		PtIntermodalRoutingModesConfigGroup.PtIntermodalRoutingModeParameterSet ptIntermodalRoutingModesParamSet
				= new PtIntermodalRoutingModesConfigGroup.PtIntermodalRoutingModeParameterSet();

		ptIntermodalRoutingModesParamSet.setDelegateMode(TransportMode.pt);
		ptIntermodalRoutingModesParamSet.setRoutingMode(artificialPtMode);

		PtIntermodalRoutingModesConfigGroup.PersonAttribute2ValuePair personAttrParamSet
				= new PtIntermodalRoutingModesConfigGroup.PersonAttribute2ValuePair();
		personAttrParamSet.setPersonFilterAttribute("canUseDrt");
		personAttrParamSet.setPersonFilterValue("true");
		ptIntermodalRoutingModesParamSet.addPersonAttribute2ValuePair(personAttrParamSet);

		ptIntermodalRoutingModesConfig.addParameterSet(ptIntermodalRoutingModesParamSet);

		controler.addOverridingModule(new PtIntermodalRoutingModesModule());

		//SRRConfigGroup needs to have the same personFilterAttr and Value as PtIntermodalRoutingModesConfigGroup
		SwissRailRaptorConfigGroup ptConfig = ConfigUtils.addOrGetModule(controler.getConfig(), SwissRailRaptorConfigGroup.class);
		ptConfig.setUseIntermodalAccessEgress(true);

		for (String drtMode : nonPtModes) {
			SwissRailRaptorConfigGroup.IntermodalAccessEgressParameterSet intermodalParamSet = new SwissRailRaptorConfigGroup.IntermodalAccessEgressParameterSet();
			intermodalParamSet.setMode(drtMode);
			ptConfig.addParameterSet(intermodalParamSet);
			intermodalParamSet.setPersonFilterAttribute("canUseDrt");
			intermodalParamSet.setPersonFilterValue("true");
			intermodalParamSet.setInitialSearchRadius(10000.);
			intermodalParamSet.setMaxRadius(10000.);
			intermodalParamSet.setSearchExtensionRadius(1000.);
			intermodalParamSet.setStopFilterAttribute("allowDrtAccessEgress");
		}

		controler.addOverridingModule(new AbstractModule() {
			@Override
			public void install() {
				bind(RaptorIntermodalAccessEgress.class).to(EnhancedRaptorIntermodalAccessEgress.class);
			}
		});

		//finally the new pt mode has to be added to subtourModeChoice
		SubtourModeChoiceConfigGroup modeChoiceConfigGroup = ConfigUtils.addOrGetModule(controler.getConfig(), SubtourModeChoiceConfigGroup.class);
		List<String> modes = new ArrayList<>();
		Collections.addAll(modes, modeChoiceConfigGroup.getModes());
		modes.add(artificialPtMode);
		modeChoiceConfigGroup.setModes(modes.toArray(new String[0]));
	}

	private record StopsControlerListener(String mode,
										  OutputDirectoryHierarchy controlerIO,
										  String stopsFile) implements StartupListener {

		private static final String OUTPUT_FILE_NAME = "stops.xml";

		@Override
		public void notifyStartup(StartupEvent event) {
			try {
				Files.copy(Path.of(stopsFile), Path.of(controlerIO.getOutputFilename(mode + "_" + OUTPUT_FILE_NAME)));
			} catch (IOException e) {
				log.fatal(e);
			}
		}
	}
}
