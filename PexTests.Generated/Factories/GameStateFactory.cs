using System;
using Microsoft.Pex.Framework;

/// <summary>A factory for GameState+PlayerId instances</summary>
public static partial class GameStateFactory {
  /// <summary>A factory for GameState+PlayerId instances</summary>
  [PexFactoryMethod(typeof(global::GameState.PlayerId))]
  public static object CreatePlayerId(int n) {
    return GameState.PlayerId.NewPlayerId(n);
  }
}
