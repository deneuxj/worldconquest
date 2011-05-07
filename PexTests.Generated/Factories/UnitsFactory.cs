using System;
using Microsoft.Pex.Framework;

/// <summary>A factory for Units+UnitInfo instances</summary>
public static partial class UnitsFactory {
  /// <summary>A factory for Units+UnitTypes instances</summary>
  [PexFactoryMethod(typeof(global::Units.UnitTypes))]
  public static object CreateUnitTypes(int t, bool isDocked, int fuel, Units.BomberTransport bt,
    Units.CarriedAircraft[] aircrafts, Units.TransportedUnit[] tu) {

    switch (t) {
      case 0: return Units.UnitTypes.Infantry;
      case 1: return Units.UnitTypes.AntiAircraft;
      case 2: return Units.UnitTypes.Artillery;
      case 3: return Units.UnitTypes.Tank;
      case 4: return Units.UnitTypes.NewBattleship(isDocked ? Units.Docked.Docked : Units.Docked.NotDocked);
      case 5:
        PexAssume.IsNotNull(bt);
        return Units.UnitTypes.NewBomber(isDocked ? Units.Landed.Landed : Units.Landed.Airborne,
                                           Units.Fuel.NewFuel(fuel), bt);
      case 6:
        PexAssume.IsNotNull(aircrafts);
        PexAssume.AreElementsNotNull(aircrafts);
        return Units.UnitTypes.NewCarrier(isDocked ? Units.Docked.Docked : Units.Docked.NotDocked,
                                                aircrafts);
      case 7:
        return Units.UnitTypes.NewDestroyer(isDocked ? Units.Docked.Docked : Units.Docked.NotDocked);

      case 8:
        return Units.UnitTypes.NewFighter(isDocked ? Units.Landed.Landed : Units.Landed.Airborne,
                                          Units.Fuel.NewFuel(fuel));

      case 9:
        return Units.UnitTypes.NewSubmarine(isDocked ? Units.Docked.Docked : Units.Docked.NotDocked, Units.Stealthy.NotStealthy);

      case 10:
        return Units.UnitTypes.NewSubmarine(isDocked ? Units.Docked.Docked : Units.Docked.NotDocked, Units.Stealthy.Stealthy);

      default:
        PexAssume.IsNotNull(tu);
        PexAssume.AreElementsNotNull(tu);
        return Units.UnitTypes.NewTransport(isDocked ? Units.Docked.Docked : Units.Docked.NotDocked, tu);
    }
  }

  /// <summary>A factory for Units+UnitInfo instances</summary>
  [PexFactoryMethod(typeof(global::Units.UnitInfo))]
  public static global::Units.UnitInfo CreateUnitInfo(
    global::HexTiling.HexCoords coords_hexCoords, float health_f,
    int t, bool isDocked, int fuel, Units.BomberTransport bt,
    Units.CarriedAircraft[] aircrafts, Units.TransportedUnit[] tu) {

    global::Units.UnitInfo unitInfo
       = new global::Units.UnitInfo(coords_hexCoords, health_f, (Units.UnitTypes)CreateUnitTypes(t, isDocked, fuel, bt, aircrafts, tu));
    return unitInfo;

    // TODO: Edit factory method of UnitInfo
    // This method should be able to configure the object in all possible ways.
    // Add as many parameters as needed,
    // and assign their values to each field by using the API.
  }
}
