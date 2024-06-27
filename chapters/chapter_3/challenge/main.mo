import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Option "mo:base/Option";
import Types "types";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Articuno Token";
    };

    public query func tokenSymbol() : async Text {
        return "AT";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance : Nat = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance : Nat = Option.get(ledger.get(owner), 0);
        if (balance < amount) {
            return #err("Insufficient balance to burn.");
        } else {
            ledger.put(owner, balance - amount);
            return #ok();
        };
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let fromBalance : Nat = Option.get(ledger.get(from), 0);
        let toBalance : Nat =Option.get(ledger.get(to), 0);
        if (fromBalance < amount) {
            return #err("Insufficient balance to transfer");
        } else {
            ledger.put(from, fromBalance - amount);
            ledger.put(to, toBalance + amount);
            return #ok();
        };
    };

    public query func balanceOf(account : Principal) : async Nat {
        return Option.get(ledger.get(account), 0);
    };

    public query func totalSupply() : async Nat {
        var totalSupply : Nat = 0;
        for (balance in ledger.vals()) {
            totalSupply += balance;
        };
        return totalSupply;
    };

};