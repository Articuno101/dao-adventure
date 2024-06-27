import Result "mo:base/Result";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Time "mo:base/Time";
import Hash "mo:base/Hash";
import Types "types";

actor {

        type Result<A, B> = Result.Result<A, B>;
        type Member = Types.Member;
        type ProposalContent = Types.ProposalContent;
        type ProposalId = Types.ProposalId;
        type Proposal = Types.Proposal;
        type ProposalStatus = Types.ProposalStatus;
        type Vote = Types.Vote;
        type HttpRequest = Types.HttpRequest;
        type HttpResponse = Types.HttpResponse;

        // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
        stable let canisterIdWebpage : Principal = Principal.fromText("z7chj-7qaaa-aaaab-qacbq-cai");
        stable var manifesto = "Let's graduate!";
        stable let name = "My DAO";
        var goals = Buffer.Buffer<Text>(0);

        let token = actor("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
                balanceOf : shared query Principal -> async Nat;
                balanceOfArray : shared query [Principal] -> async [Nat];
                burn : shared (Principal, Nat) -> async Result<(), Text>;
                mint : shared (Principal, Nat) -> async Result<(), Text>;
                tokenName : shared query () -> async Text;
                tokenSymbol : shared query () -> async Text;
                totalSupply : shared query () -> async Nat;
                transfer : shared (Principal, Principal, Nat) -> async Result<(), Text>;
        };

        let webpage = actor("uy3uz-syaaa-aaaab-qadka-cai") : actor {
                http_request: HttpRequest -> async HttpResponse;
                setManifesto: Text -> async Result<(), Text>
        };

        // Returns the name of the DAO
        public query func getName() : async Text {
                return name;
        };

        // Returns the manifesto of the DAO
        public query func getManifesto() : async Text {
                return manifesto;
        };

        // Returns the goals of the DAO
        public query func getGoals() : async [Text] {
                return Buffer.toArray(goals);
        };

        let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
        let firstMember : Member = {
                name = "motoko_bootcamp";
                role = #Mentor;
        };

        members.put(Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai"), firstMember);

        // Register a new member in the DAO with the given name and principal of the caller
        // Airdrop 10 MBC tokens to the new member
        // New members are always Student
        // Returns an error if the member already exists
        public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case(null) {
                                members.put(caller, member);
                                let _ = await token.mint(caller, 10);
                                return #ok();
                        };
                        case(_) {
                                return #err("Member already exists");
                        };
                };
        };

        // Get the member with the given principal
        // Returns an error if the member does not exist
        public query func getMember(p : Principal) : async Result<Member, Text> {
                switch (members.get(p)) {
                        case(null) {
                                return #err("Member does not exist.");
                        };
                        case(?member) {
                                return #ok(member);
                        };
                };
        };

        private func isMentor(member : Member) : Bool {
                return member.role == #Mentor;
        };

        // Graduate the student with the given principal
        // Returns an error if the student does not exist or is not a student
        // Returns an error if the caller is not a mentor
        public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
                let getStudent = await getMember(student);
                switch(getStudent) {
                        case(#err(message)) {
                               return #err(message); 
                        };
                        case(#ok(member)) {
                                if(member.role != #Student) {
                                        return #err("Member is not a student.");
                                };
                                let getMentor = await getMember(caller);
                                switch(getMentor) {
                                        case(#err(message)) {
                                                return #err(message); 
                                        };
                                        case(#ok(mentor)) {
                                                if(not isMentor(mentor)) {
                                                        return #err("Caller is not a mentor");
                                                };
                                                let newMember : Member = {
                                                        name = member.name;
                                                        role = #Graduate;
                                                };
                                                members.put(student, newMember);
                                                return #ok();
                                        };
                                };
                        };
                };
        };

        let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, func (id : Nat) : Nat32 {Hash.hash(id % 23)});
        var newProposalID : Nat = 0;

        // Create a new proposal and returns its id
        // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
        public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
                let getMentor = await getMember(caller);
                switch(getMentor) {
                        case(#err(message)) {
                                return #err(message); 
                        };
                        case(#ok(mentor)) {
                                if(not isMentor(mentor)) {
                                        return #err("Caller is not a mentor");
                                };
                                switch(await token.burn(caller, 1)) {
                                        case(#err(message)) {
                                                return #err(message);
                                        };
                                        case(#ok) {
                                                let newProposal : Proposal = {
                                                        id = newProposalID;
                                                        content = content;
                                                        creator = caller;
                                                        created = Time.now();
                                                        executed = null;
                                                        votes = [];
                                                        voteScore = 0;
                                                        status = #Open;
                                                };
                                                proposals.put(newProposalID, newProposal);
                                                newProposalID += 1;
                                                return #ok(newProposal.id);
                                        };
                                };
                        };
                };
        };

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch(proposals.get(id)) {
                        case(null) {
                                return #err("Proposal does not exist.");
                        };
                        case(?proposal) {
                                return #ok(proposal);
                        };
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Iter.toArray<Proposal>(proposals.vals());
        };

        private func executeProposal(proposal: Proposal) : async () {
                switch(proposal.content) {
                        case(#ChangeManifesto(newManifesto)) {
                                manifesto := newManifesto;
                                let _ = webpage.setManifesto(newManifesto);
                                return();
                        };
                        case(#AddMentor(principal)) {
                                let getmember = await getMember(principal);
                                switch(getmember) {
                                        case(#err(_)) {
                                                return();
                                        };
                                        case(#ok(member)) {
                                                if (member.role != #Graduate) {
                                                        return();
                                                };
                                                let updatedMember : Member = {
                                                        name = member.name;
                                                        role = #Mentor;
                                                };
                                                members.put(principal, updatedMember);
                                                return();
                                        };
                                };
                        };
                        case(#AddGoal(goal)) {
                                goals.add(goal);
                        };
                };
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                let getproposal = await getProposal(proposalId);
                switch(getproposal) {
                        case(#err(message)) {
                                return #err(message);
                        };
                        case(#ok(proposal)) {
                                let getmember = await getMember(caller);
                                switch(getmember) {
                                        case(#err(message)) {
                                                return #err(message);
                                        };
                                        case(#ok(member)) {
                                                if(proposal.status != #Open) {
                                                        return #err("The proposal is not open.");
                                                };
                                                if((member.role != #Graduate) and (member.role != #Mentor)) {
                                                        return #err("Member is not allowe to vote;");
                                                };

                                                let newVotingPower : Nat = switch(member.role) {
                                                        case(#Graduate) {
                                                                await token.balanceOf(caller);
                                                        };
                                                        case(#Mentor) {
                                                                let calculation = await token.balanceOf(caller);
                                                                calculation * 5;
                                                        };
                                                        case(#Student) {
                                                                0;
                                                        };
                                                };

                                                let vote : Vote = {
                                                        member = caller;
                                                        votingPower = newVotingPower;
                                                        yesOrNo = yesOrNo;
                                                };

                                                let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                                                newVotes.add(vote);

                                                let multiplier : Int = switch(vote.yesOrNo) {
                                                        case(true) {
                                                                1;
                                                        };
                                                        case(false) {
                                                                -1;
                                                        };
                                                };

                                                let newVoteScore : Int = proposal.voteScore + multiplier * vote.votingPower;

                                                let newStatus : ProposalStatus = if(newVoteScore > 100) {
                                                        #Accepted;
                                                } else if(newVoteScore < 100) {
                                                        #Rejected;
                                                } else {
                                                        #Open;
                                                };

                                                let newExecuted : ?Time.Time = switch(newStatus) {
                                                        case(#Accepted) {
                                                                let _ = executeProposal(proposal);
                                                                ?Time.now();
                                                        };
                                                        case(_) {
                                                                null;
                                                        };
                                                };

                                                let newProposal : Proposal = {
                                                        id = proposal.id;
                                                        content = proposal.content;
                                                        creator = proposal.creator;
                                                        created = proposal.created;
                                                        executed = newExecuted;
                                                        votes = Buffer.toArray<Vote>(newVotes);
                                                        voteScore = newVoteScore;
                                                        status = newStatus;
                                                };
                                                proposals.put(proposalId, newProposal);
                                                return #ok();
                                        };
                                };
                        };
                };
        };

        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
