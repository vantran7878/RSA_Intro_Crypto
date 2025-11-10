#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <chrono> 

// using std::cout;
// using std::endl;
// using std::chrono::high_resolution_clock;
// using std::chrono::duration_cast;
// using std::chrono::microseconds;
// using std::chrono::milliseconds;

using namespace std;

struct GcdResult;

class BigInt {
    private:
        vector<int> bin;
        bool isNegative;
    public:
        void removeLeadingZeros();
        BigInt();
        BigInt(string hexStr);
    
        bool isMagnitudeLessThan(const BigInt& other) const;
        static BigInt addMagnitude(const BigInt& A, const BigInt& B);
        static BigInt subtractMagnitude(const BigInt& A, const BigInt& B);
        static BigInt multiplyMagnitude(const BigInt& A, const BigInt& B);
        
        BigInt operator+(const BigInt& other) const;
        BigInt operator-(const BigInt& other) const;
        BigInt operator*(const BigInt& other) const;
        BigInt operator/(const BigInt& other) const;
        BigInt operator%(const BigInt& other) const;
        bool operator>(const BigInt& other) const;
        bool operator!=(const BigInt& other) const;
        bool operator==(const BigInt& other) const;

        static GcdResult extendedEuclidean(BigInt a, BigInt b);
        static BigInt modInverse(BigInt e, BigInt phi);
        string toHex();
        string toBin();
};

struct GcdResult {
    BigInt gcd; 
    BigInt x;   
    BigInt y;   
};

void BigInt::removeLeadingZeros() {
        while (bin.size() > 1 && !bin[0]) {
            bin.erase(bin.begin()); 
        }
        if (bin.size() == 1 && !bin[0]) {
            isNegative = false;
        }
    }

BigInt::BigInt() : isNegative(false) {
    bin.push_back(0); 
}

BigInt::BigInt(string hexStr) : isNegative(0) {
    if (hexStr.empty()) {
        return;
    }
    int len = hexStr.size();
    for (int i = len - 1; i >= 0; i--) {
        switch (hexStr[i])
        {
        case '0': this->bin.insert(bin.end(), {0, 0, 0, 0}); break;
        case '1': this->bin.insert(bin.end(), {0, 0, 0, 1}); break;
        case '2': this->bin.insert(bin.end(), {0, 0, 1, 0}); break;
        case '3': this->bin.insert(bin.end(), {0, 0, 1, 1}); break;
        case '4': this->bin.insert(bin.end(), {0, 1, 0, 0}); break;
        case '5': this->bin.insert(bin.end(), {0, 1, 0, 1}); break;
        case '6': this->bin.insert(bin.end(), {0, 1, 1, 0}); break;
        case '7': this->bin.insert(bin.end(), {0, 1, 1, 1}); break;
        case '8': this->bin.insert(bin.end(), {1, 0, 0, 0}); break;
        case '9': this->bin.insert(bin.end(), {1, 0, 0, 1}); break;
        case 'A': this->bin.insert(bin.end(), {1, 0, 1, 0}); break;
        case 'B': this->bin.insert(bin.end(), {1, 0, 1, 1}); break;
        case 'C': this->bin.insert(bin.end(), {1, 1, 0, 0}); break;
        case 'D': this->bin.insert(bin.end(), {1, 1, 0, 1}); break;
        case 'E': this->bin.insert(bin.end(), {1, 1, 1, 0}); break;
        case 'F': this->bin.insert(bin.end(), {1, 1, 1, 1}); break;
        default:
            break;
        }
    }
   
    removeLeadingZeros();

}

string BigInt::toBin() {
    if (bin.empty()) {
        return "0";
    }
    string binStr = "";
    for (bool i : this->bin) {
        binStr += to_string(i);
    }
    return binStr;
}

string BigInt::toHex() {
    if (bin.empty()) {
        return "0";
    }

    string hexStr = "";
    const string hexChars = "0123456789ABCDEF";
   
    while (bin.size() % 4 != 0) {
        bin.insert(bin.begin(), 0);
    }

    for (int i = 0; i < bin.size(); i+=4) {
        int val = 0;
        val += this->bin[i] * 8;
        val += this->bin[i + 1] * 4;
        val += this->bin[i + 2] * 2;
        val += this->bin[i + 3] * 1;
        
        hexStr.push_back(hexChars[val]);
    }

    return hexStr;

}

bool BigInt::isMagnitudeLessThan(const BigInt& other) const {
    if (this->bin.size() != other.bin.size()) {
        return this->bin.size() < other.bin.size();
    }

    for (size_t i = 0; i < bin.size(); i++) {
        if (this->bin[i] != other.bin[i]) {
            return !this->bin[i] && other.bin[i]; 
        }
    }
    return false;
}

BigInt BigInt::addMagnitude(const BigInt& A, const BigInt& B) { 
    BigInt result;
    int carry = 0;

    int i = (int)A.bin.size() - 1;
    int j = (int)B.bin.size() - 1;
    
    int maxLen = 1 + max(A.bin.size(), B.bin.size());
    result.bin.assign(maxLen, 0);
    int k = maxLen - 1;

    while (i >= 0 || j >= 0 || carry) {
        int bitA = (i >= 0) ? A.bin[i] : 0;
        int bitB = (j >= 0) ? B.bin[j] : 0;

        int sum = bitA + bitB + carry; 
        
        result.bin[k] = sum % 2; 
        carry = sum / 2;

        i--;
        j--;
        k--;
    }
    
    result.removeLeadingZeros(); 
    return result;
}

BigInt BigInt::subtractMagnitude(const BigInt& A, const BigInt& B) {
    BigInt result;
    int borrow = 0;
    
    // Cấp phát trước với kích thước của A (vì A >= B)
    result.bin.assign(A.bin.size(), 0);

    int i = (int)A.bin.size() - 1;
    int j = (int)B.bin.size() - 1;
    int k = (int)result.bin.size() - 1;

    while (i >= 0) {
        int bitA = A.bin[i];
        int bitB = (j >= 0) ? B.bin[j] : 0;
       
        int diff = bitA - bitB - borrow;
        if (diff < 0) {
            diff += 2;
            borrow = 1;
        } else {
            borrow = 0;
        }
        
        result.bin[k] = diff; 

        i--;
        j--;
        k--;
    }

    result.removeLeadingZeros(); 
    return result;
}

BigInt BigInt::multiplyMagnitude(const BigInt& A, const BigInt& B) {
    BigInt result("0");
    int pad = 0; 

    for (int j = B.bin.size() - 1; j >= 0; j--) {
        if (B.bin[j] == 1) {
            BigInt tmp = A;
            for (int i = 0; i < pad; i++) {
                tmp.bin.push_back(0);
            }
            result = result + tmp; 
        }
        pad++;
    }
    return result;
}

BigInt BigInt::operator*(const BigInt& other) const {

    if ((this->bin.size() == 1 && this->bin[0] == 0) || (other.bin.size() == 1 && other.bin[0] == 0)) {
        return BigInt("0");
    }

    BigInt result;

    if (this->isMagnitudeLessThan(other)) {
        result = multiplyMagnitude(other, *this);
    } else {
        result = multiplyMagnitude(*this, other); 
    }

    // Dấu kết quả = (dấu A) XOR (dấu B)
    result.isNegative = this->isNegative ^ other.isNegative; 
    result.removeLeadingZeros(); 
    return result;
}

BigInt BigInt::operator+(const BigInt& other) const {
   BigInt result;
    if (!this->isNegative && !other.isNegative) {
        // A + B ==> A + B
        result = addMagnitude(*this, other);
        result.isNegative = false;
    }
    else if (this->isNegative && other.isNegative) {
        // -A + -B == > -(A + B)
        result = addMagnitude(*this, other);
        result.isNegative = true;
    }
    else if (!this->isNegative && other.isNegative) {
        // A + (-B)  => A - B
        BigInt tempOther = other;
        tempOther.isNegative = false; 
        result = *this - tempOther; 
  
    } else { 
        // (-A) + B => B - A
        BigInt tempThis = *this;
        tempThis.isNegative = false;
        result = other - tempThis; 
    }
    
    result.removeLeadingZeros();
    return result;
}

BigInt BigInt::operator-(const BigInt& other) const {
    BigInt result;
    if (!this->isNegative && other.isNegative) {
        result = addMagnitude(*this, other);
        result.isNegative = false;
    }
    else if (this->isNegative && !other.isNegative) {
        result = addMagnitude(*this, other);
        result.isNegative = true;
    }
    else {
        if (this->isMagnitudeLessThan(other)) {
            result = subtractMagnitude(other, *this);
           
            result.isNegative = !this->isNegative; 
        } else {
            result = subtractMagnitude(*this, other);
          
            result.isNegative = this->isNegative;
        }
    }
    result.removeLeadingZeros();
    return result;
}

BigInt BigInt::operator/(const BigInt& other) const {
    if (other.bin.size() == 1 && other.bin[0] == 0) {
        throw std::runtime_error("Division by zero");
    }
    
    if (this->bin.size() == 1 && this->bin[0] == 0) {
        return BigInt("0");
    }
    
    BigInt A_magnitude = *this; A_magnitude.isNegative = false;
    BigInt B_magnitude = other; B_magnitude.isNegative = false;
    if (A_magnitude.isMagnitudeLessThan(B_magnitude)) {
        return BigInt("0");
    }

    BigInt quotient; 
    quotient.bin.clear(); 
    BigInt remainder("0");
   
    for (size_t i = 0; i < this->bin.size(); i++) {
        
        if (!(remainder.bin.size() == 1 && remainder.bin[0] == 0)) {
             remainder.bin.push_back(0); 
        }
       
        remainder.bin.back() = this->bin[i]; 

        if (remainder.isMagnitudeLessThan(B_magnitude)) {
            quotient.bin.push_back(0);
        } else {
            quotient.bin.push_back(1); 
            remainder = subtractMagnitude(remainder, B_magnitude);
        }
    }
    
    if (quotient.bin.empty()) {
        quotient.bin.push_back(0);
    }
    
    quotient.removeLeadingZeros();
    
   
    quotient.isNegative = this->isNegative ^ other.isNegative;

    return quotient;
}

BigInt BigInt::operator%(const BigInt& other) const {
    // 1. Chia cho 0
    if (other.bin.size() == 1 && other.bin[0] == 0) {
        throw std::runtime_error("Division by zero (modulo)");
    }
    
    // 2. A == 0
    if (this->bin.size() == 1 && this->bin[0] == 0) {
        return BigInt("0"); // 0 % B = 0
    }
    
    // 3. |A| < |B|
    BigInt A_magnitude = *this; A_magnitude.isNegative = false;
    BigInt B_magnitude = other; B_magnitude.isNegative = false;
    if (A_magnitude.isMagnitudeLessThan(B_magnitude)) {
        return *this;
    }

  
    BigInt remainder("0");

    for (size_t i = 0; i < this->bin.size(); i++) {
    
        if (!(remainder.bin.size() == 1 && remainder.bin[0] == 0)) {
             remainder.bin.push_back(0);
        }
       
        remainder.bin.back() = this->bin[i]; 

        if (!remainder.isMagnitudeLessThan(B_magnitude)) {
            remainder = subtractMagnitude(remainder, B_magnitude);
        }
    }
    

    remainder.removeLeadingZeros();

    remainder.isNegative = this->isNegative;

    return remainder;
}

bool BigInt::operator!=(const BigInt& other) const {
   
    if (this->isNegative != other.isNegative) return true;
    if (this->bin.size() != other.bin.size()) return true;
    
    for (size_t i = 0; i < this->bin.size(); ++i) {
        if (this->bin[i] != other.bin[i]) return true;
    }
    
    return false; 
}

bool BigInt::operator==(const BigInt& other) const {
    return !(*this != other);
}

bool BigInt::operator>(const BigInt& other) const {
    if (this->isNegative != other.isNegative) {
        return !this->isNegative;
    }

    if (this->isNegative) {
        return this->isMagnitudeLessThan(other);
    }
    return !this->isMagnitudeLessThan(other) && (*this != other);
}

GcdResult BigInt::extendedEuclidean(BigInt a, BigInt b) {
    BigInt x0("1"), x1("0");
    BigInt y0("0"), y1("1");
    BigInt zero("0");

    a.isNegative = false;
    b.isNegative = false;

    while (b > zero) { 
        BigInt q = a / b;
        BigInt r = a % b;
        
        a = b;
        b = r;

        BigInt tempX = x1;
        x1 = x0 - q * x1;
        x0 = tempX;

        BigInt tempY = y1;
        y1 = y0 - q * y1;
        y0 = tempY;
    }
    
    return {a, x0, y0}; 
}

BigInt BigInt::modInverse(BigInt e, BigInt phi) {
    GcdResult res = extendedEuclidean(e, phi);
    
    BigInt one("1");
    BigInt negOne("-1");

    if (res.gcd != one) {
        return negOne; 
    }

    BigInt d = res.x;
    
    d = (d % phi + phi) % phi;

    return d;
}

int main(int argc, char *argv[]) {
    // auto start = high_resolution_clock::now();
    ifstream in(argv[1]);
    ofstream out(argv[2]);

    if (!in.is_open() || !out.is_open()) {
        cerr << "Error opening file(s)" << endl;
        return 1;
    }

    string p_hex, q_hex, e_hex;
    getline(in, p_hex);
    getline(in, q_hex);
    getline(in, e_hex);

    try {
        BigInt p(p_hex);
        BigInt q(q_hex);
        BigInt e(e_hex);

        BigInt one("1");
        BigInt negOne("-1");

        // 1. Tính phi = (p-1) * (q-1)
        BigInt p_minus_1 = p - one;
        BigInt q_minus_1 = q - one;
        BigInt phi = p_minus_1 * q_minus_1;

        // 2. Tính d = e^(-1) mod phi
        BigInt d = BigInt::modInverse(e, phi);

        // 3. Kiểm tra và ghi kết quả
        if (d == negOne) {
            out << -1 << endl;
        } else {
            string d_hex = d.toHex();
            out << d_hex << endl;
        }

    } catch (const exception& ex) {
        cerr << "An error occurred: " << ex.what() << endl;
        out << -1 << endl;
        return 1;
    }

    in.close();
    out.close();

    // auto stop = high_resolution_clock::now();
    // // Tính theo micro giây (một phần triệu giây)
    // auto duration_us = duration_cast<microseconds>(stop - start);
    
    // // Tính theo mili giây (một phần nghìn giây)
    // auto duration_ms = duration_cast<milliseconds>(stop - start);

    // cout << "--- Thoi gian chay ---" << endl;
    // cout << "Microseconds: " << duration_us.count() << " micros" << endl;
    // cout << "Milliseconds: " << duration_ms.count() << " ms" << endl;
    return 0;
}