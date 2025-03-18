import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const wallet1 = accounts.get("wallet_1")!;

describe("smart wallet savings", () => {
  it("creates a savings goal", () => {
    const createGoal = simnet.callPublicFn(
      "wallet",
      "create-goal",
      [Cl.uint(1000), Cl.stringAscii("Vacation")],
      wallet1
    );
    expect(createGoal.result).toBeOk(Cl.bool(true));
  });



  it("prevents withdrawal before reaching goal", () => {
    const withdraw = simnet.callPublicFn(
      "wallet",
      "withdraw",
      [Cl.uint(100), Cl.uint(1)], // Added goal-id parameter
      wallet1
    );
    expect(withdraw.result).toBeErr(Cl.uint(102));
});




});
